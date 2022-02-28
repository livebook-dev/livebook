defmodule Livebook.Runtime.ErlDist.RuntimeServer do
  @moduledoc false

  # A server process backing a specific runtime.
  #
  # This process handles `Livebook.Runtime` operations,
  # like evaluation and completion. It spawns/terminates
  # individual evaluators corresponding to evaluation
  # containers as necessary.
  #
  # Every runtime server must have an owner process,
  # to which the server lifetime is bound.
  #
  # For more specification see `Livebook.Runtime`.

  use GenServer, restart: :temporary

  alias Livebook.Runtime.Evaluator
  alias Livebook.Runtime
  alias Livebook.Runtime.ErlDist

  @await_owner_timeout 5_000
  @memory_usage_interval 15_000

  @doc """
  Starts the manager.

  Note: make sure to call `attach` within #{@await_owner_timeout}ms
  or the runtime server assumes it's not needed and terminates.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Sets the owner process.

  The owner process is monitored and as soon as it terminates,
  the server also terminates. All the evaluation results are
  send directly to the owner.

  ## Options

  See `Livebook.Runtime.connect/2` for the list of available
  options.
  """
  @spec attach(pid(), pid(), keyword()) :: :ok
  def attach(pid, owner, opts \\ []) do
    GenServer.cast(pid, {:attach, owner, opts})
  end

  @doc """
  Evaluates the given code using an `Livebook.Runtime.Evaluator`
  process belonging to the given container and instructs
  it to send all the outputs to the owner process.

  If no evaluator exists for the given container, a new
  one is started.

  See `Livebook.Runtime.Evaluator` for more details.
  """
  @spec evaluate_code(pid(), String.t(), Runtime.locator(), Runtime.locator(), keyword()) :: :ok
  def evaluate_code(pid, code, locator, prev_locator, opts \\ []) do
    GenServer.cast(pid, {:evaluate_code, code, locator, prev_locator, opts})
  end

  @doc """
  Removes the specified evaluation from the history.

  See `Livebook.Runtime.Evaluator` for more details.
  """
  @spec forget_evaluation(pid(), Runtime.locator()) :: :ok
  def forget_evaluation(pid, locator) do
    GenServer.cast(pid, {:forget_evaluation, locator})
  end

  @doc """
  Terminates the `Livebook.Runtime.Evaluator` process that belongs
  to the given container.
  """
  @spec drop_container(pid(), Runtime.container_ref()) :: :ok
  def drop_container(pid, container_ref) do
    GenServer.cast(pid, {:drop_container, container_ref})
  end

  @doc """
  Asynchronously sends an intellisense request to the server.

  Completions are forwarded to `Livebook.Runtime.Evaluator` process
  that belongs to the given container. If there's no evaluator,
  there's also no binding and environment, so a generic
  completion is handled by a temporary process.

  See `Livebook.Runtime` for more details.
  """
  @spec handle_intellisense(
          pid(),
          pid(),
          reference(),
          Runtime.intellisense_request(),
          Runtime.locator()
        ) :: :ok
  def handle_intellisense(pid, send_to, ref, request, locator) do
    GenServer.cast(pid, {:handle_intellisense, send_to, ref, request, locator})
  end

  @doc """
  Reads file at the given absolute path within the runtime
  file system.
  """
  @spec read_file(pid(), String.t()) :: {:ok, binary()} | {:error, String.t()}
  def read_file(pid, path) do
    {result_ref, task_pid} = GenServer.call(pid, {:read_file, path})

    monitor_ref = Process.monitor(task_pid)

    receive do
      {:result, ^result_ref, result} ->
        result

      {:DOWN, ^monitor_ref, :process, _object, _reason} ->
        {:error, "unexpected termination"}
    end
  end

  @doc """
  Starts a new smart cell.
  """
  @spec start_smart_cell(pid(), String.t(), String.t(), term()) :: :ok
  def start_smart_cell(pid, kind, ref, attrs) do
    GenServer.cast(pid, {:start_smart_cell, kind, ref, attrs})
  end

  @doc """
  Stops the given smart cell.
  """
  @spec stop_smart_cell(pid(), String.t()) :: :ok
  def stop_smart_cell(pid, ref) do
    GenServer.cast(pid, {:stop_smart_cell, ref})
  end

  @doc """
  Stops the manager.

  This results in all Livebook-related modules being unloaded
  from the runtime node.
  """
  @spec stop(pid()) :: :ok
  def stop(pid) do
    GenServer.stop(pid)
  end

  @impl true
  def init(_opts) do
    Process.send_after(self(), :check_owner, @await_owner_timeout)
    schedule_memory_usage_report()

    {:ok, evaluator_supervisor} = ErlDist.EvaluatorSupervisor.start_link()
    {:ok, task_supervisor} = Task.Supervisor.start_link()
    {:ok, object_tracker} = Livebook.Runtime.Evaluator.ObjectTracker.start_link()

    {:ok,
     %{
       owner: nil,
       runtime_broadcast_to: nil,
       evaluators: %{},
       evaluator_supervisor: evaluator_supervisor,
       task_supervisor: task_supervisor,
       object_tracker: object_tracker,
       smart_cell_supervisor: nil,
       smart_cell_gl: nil,
       smart_cells: %{},
       smart_cell_definitions: [],
       memory_timer_ref: nil
     }}
  end

  @impl true
  def handle_info(:check_owner, state) do
    # If not owner has been set within @await_owner_timeout
    # from the start, terminate the process.
    if state.owner do
      {:noreply, state}
    else
      {:stop, :no_owner, state}
    end
  end

  def handle_info({:DOWN, _, :process, owner, _}, %{owner: owner} = state) do
    {:stop, :shutdown, state}
  end

  def handle_info({:DOWN, _, :process, pid, reason}, state) do
    state.evaluators
    |> Enum.find(fn {_container_ref, evaluator} ->
      evaluator.pid == pid
    end)
    |> case do
      {container_ref, _} ->
        message = Exception.format_exit(reason)
        send(state.owner, {:runtime_container_down, container_ref, message})
        {:noreply, %{state | evaluators: Map.delete(state.evaluators, container_ref)}}

      nil ->
        {:noreply, state}
    end
  end

  def handle_info({:evaluation_finished, _ref}, state) do
    {:noreply, report_small_cell_definitions(state)}
  end

  def handle_info(:memory_usage, state) do
    report_memory_usage(state)
    schedule_memory_usage_report()
    {:noreply, state}
  end

  def handle_info(_message, state), do: {:noreply, state}

  @impl true
  def handle_cast({:attach, owner, opts}, state) do
    if state.owner do
      raise "runtime owner has already been configured"
    end

    Process.monitor(owner)

    state = %{state | owner: owner, runtime_broadcast_to: opts[:runtime_broadcast_to]}
    state = report_small_cell_definitions(state)
    report_memory_usage(state)

    {:ok, smart_cell_supervisor} = DynamicSupervisor.start_link(strategy: :one_for_one)
    {:ok, smart_cell_gl} = ErlDist.SmartCellGL.start_link(state.runtime_broadcast_to)
    Process.group_leader(smart_cell_supervisor, smart_cell_gl)

    {:noreply,
     %{state | smart_cell_supervisor: smart_cell_supervisor, smart_cell_gl: smart_cell_gl}}
  end

  def handle_cast(
        {:evaluate_code, code, {container_ref, evaluation_ref}, prev_locator, opts},
        state
      ) do
    state = ensure_evaluator(state, container_ref)

    prev_evaluation_ref =
      case prev_locator do
        {^container_ref, evaluation_ref} ->
          evaluation_ref

        {parent_container_ref, evaluation_ref} ->
          Evaluator.initialize_from(
            state.evaluators[container_ref],
            state.evaluators[parent_container_ref],
            evaluation_ref
          )

          nil
      end

    opts = Keyword.put(opts, :notify_to, self())

    Evaluator.evaluate_code(
      state.evaluators[container_ref],
      code,
      evaluation_ref,
      prev_evaluation_ref,
      opts
    )

    {:noreply, state}
  end

  def handle_cast({:forget_evaluation, {container_ref, evaluation_ref}}, state) do
    with {:ok, evaluator} <- Map.fetch(state.evaluators, container_ref) do
      Evaluator.forget_evaluation(evaluator, evaluation_ref)
    end

    {:noreply, state}
  end

  def handle_cast({:drop_container, container_ref}, state) do
    state = discard_evaluator(state, container_ref)
    {:noreply, state}
  end

  def handle_cast({:handle_intellisense, send_to, ref, request, locator}, state) do
    {container_ref, evaluation_ref} = locator
    evaluator = state.evaluators[container_ref]

    intellisense_context =
      if evaluator == nil or elem(request, 0) in [:format] do
        Evaluator.intellisense_context()
      else
        Evaluator.intellisense_context(evaluator, evaluation_ref)
      end

    Task.Supervisor.start_child(state.task_supervisor, fn ->
      response = Livebook.Intellisense.handle_request(request, intellisense_context)
      send(send_to, {:runtime_intellisense_response, ref, request, response})
    end)

    {:noreply, state}
  end

  def handle_cast({:start_smart_cell, kind, ref, attrs}, state) do
    definition = Enum.find(state.smart_cell_definitions, &(&1.kind == kind))

    state =
      case DynamicSupervisor.start_child(
             state.smart_cell_supervisor,
             {definition.module, %{ref: ref, attrs: attrs, target_pid: state.owner}}
           ) do
        {:ok, pid, info} ->
          send(state.owner, {:runtime_smart_cell_started, ref, info})
          put_in(state.smart_cells[ref], pid)

        _ ->
          state
      end

    {:noreply, state}
  end

  def handle_cast({:stop_smart_cell, ref}, state) do
    {pid, state} = pop_in(state.smart_cells[ref])

    if pid do
      DynamicSupervisor.terminate_child(state.smart_cell_supervisor, pid)
    end

    {:noreply, state}
  end

  @impl true
  def handle_call({:read_file, path}, {from_pid, _}, state) do
    # Delegate reading to a separate task and let the caller
    # wait for the response

    result_ref = make_ref()

    {:ok, task_pid} =
      Task.Supervisor.start_child(state.task_supervisor, fn ->
        result =
          case File.read(path) do
            {:ok, content} -> {:ok, content}
            {:error, posix} -> {:error, posix |> :file.format_error() |> List.to_string()}
          end

        send(from_pid, {:result, result_ref, result})
      end)

    {:reply, {result_ref, task_pid}, state}
  end

  defp ensure_evaluator(state, container_ref) do
    if Map.has_key?(state.evaluators, container_ref) do
      state
    else
      {:ok, evaluator} =
        ErlDist.EvaluatorSupervisor.start_evaluator(
          state.evaluator_supervisor,
          send_to: state.owner,
          runtime_broadcast_to: state.runtime_broadcast_to,
          object_tracker: state.object_tracker
        )

      Process.monitor(evaluator.pid)
      %{state | evaluators: Map.put(state.evaluators, container_ref, evaluator)}
    end
  end

  defp discard_evaluator(state, container_ref) do
    case Map.fetch(state.evaluators, container_ref) do
      {:ok, evaluator} ->
        ErlDist.EvaluatorSupervisor.terminate_evaluator(state.evaluator_supervisor, evaluator)
        %{state | evaluators: Map.delete(state.evaluators, container_ref)}

      :error ->
        state
    end
  end

  defp schedule_memory_usage_report() do
    Process.send_after(self(), :memory_usage, @memory_usage_interval)
  end

  defp report_memory_usage(%{owner: nil}), do: :ok

  defp report_memory_usage(state) do
    send(state.owner, {:runtime_memory_usage, Evaluator.memory()})
  end

  defp report_small_cell_definitions(state) do
    smart_cell_definitions = get_smart_cell_definitions()

    if smart_cell_definitions == state.smart_cell_definitions do
      state
    else
      defs = Enum.map(smart_cell_definitions, &Map.take(&1, [:kind, :name]))
      send(state.owner, {:runtime_smart_cell_definitions, defs})
      %{state | smart_cell_definitions: smart_cell_definitions}
    end
  end

  @compile {:no_warn_undefined, {Kino.SmartCell, :definitions, 0}}

  defp get_smart_cell_definitions() do
    if Code.ensure_loaded?(Kino.SmartCell) and function_exported?(Kino.SmartCell, :definitions, 0) do
      Kino.SmartCell.definitions()
    else
      []
    end
  end
end
