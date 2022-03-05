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

  require Logger

  alias Livebook.Runtime.Evaluator
  alias Livebook.Runtime
  alias Livebook.Runtime.ErlDist

  @await_owner_timeout 5_000
  @memory_usage_interval 15_000

  @doc """
  Starts the manager.

  Note: make sure to call `attach` within #{@await_owner_timeout}ms
  or the runtime server assumes it's not needed and terminates.

  ## Options

    * `:smart_cell_definitions_module` - the module to read smart
      cell definitions from, it needs to export a `definitions/0`
      function. Defaults to `Kino.SmartCell`
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
  def evaluate_code(pid, code, locator, base_locator, opts \\ []) do
    GenServer.cast(pid, {:evaluate_code, code, locator, base_locator, opts})
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
  def handle_intellisense(pid, send_to, ref, request, base_locator) do
    GenServer.cast(pid, {:handle_intellisense, send_to, ref, request, base_locator})
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
  @spec start_smart_cell(
          pid(),
          String.t(),
          Runtime.smart_cell_ref(),
          Runtime.smart_cell_attrs(),
          Runtime.locator()
        ) :: :ok
  def start_smart_cell(pid, kind, ref, attrs, base_locator) do
    GenServer.cast(pid, {:start_smart_cell, kind, ref, attrs, base_locator})
  end

  @doc """
  Updates the locator with smart cell context.
  """
  @spec set_smart_cell_base_locator(pid(), Runtime.smart_cell_ref(), Runtime.locator()) :: :ok
  def set_smart_cell_base_locator(pid, ref, base_locator) do
    GenServer.cast(pid, {:set_smart_cell_base_locator, ref, base_locator})
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
  def init(opts) do
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
       smart_cell_definitions_module:
         Keyword.get(opts, :smart_cell_definitions_module, Kino.SmartCell),
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

  def handle_info({:DOWN, _, :process, _, _} = message, state) do
    {:noreply,
     state
     |> handle_down_evaluator(message)
     |> handle_down_scan_binding(message)}
  end

  def handle_info({:evaluation_finished, pid, evaluation_ref}, state) do
    {:noreply,
     state
     |> report_smart_cell_definitions()
     |> scan_binding_after_evaluation(pid, evaluation_ref)}
  end

  def handle_info(:memory_usage, state) do
    report_memory_usage(state)
    schedule_memory_usage_report()
    {:noreply, state}
  end

  def handle_info({:scan_binding_ack, ref}, state) do
    {:noreply, finish_scan_binding(ref, state)}
  end

  def handle_info(_message, state), do: {:noreply, state}

  defp handle_down_evaluator(state, {:DOWN, _, :process, pid, reason}) do
    state.evaluators
    |> Enum.find(fn {_container_ref, evaluator} -> evaluator.pid == pid end)
    |> case do
      {container_ref, _} ->
        message = Exception.format_exit(reason)
        send(state.owner, {:runtime_container_down, container_ref, message})
        %{state | evaluators: Map.delete(state.evaluators, container_ref)}

      nil ->
        state
    end
  end

  defp handle_down_scan_binding(state, {:DOWN, monitor_ref, :process, _, _}) do
    Enum.find_value(state.smart_cells, fn
      {ref, %{scan_binding_monitor_ref: ^monitor_ref}} -> ref
      _ -> nil
    end)
    |> case do
      nil -> state
      ref -> finish_scan_binding(ref, state)
    end
  end

  @impl true
  def handle_cast({:attach, owner, opts}, state) do
    if state.owner do
      raise "runtime owner has already been configured"
    end

    Process.monitor(owner)

    state = %{state | owner: owner, runtime_broadcast_to: opts[:runtime_broadcast_to]}
    state = report_smart_cell_definitions(state)
    report_memory_usage(state)

    {:ok, smart_cell_supervisor} = DynamicSupervisor.start_link(strategy: :one_for_one)
    {:ok, smart_cell_gl} = ErlDist.SmartCellGL.start_link(state.runtime_broadcast_to)
    Process.group_leader(smart_cell_supervisor, smart_cell_gl)

    {:noreply,
     %{state | smart_cell_supervisor: smart_cell_supervisor, smart_cell_gl: smart_cell_gl}}
  end

  def handle_cast(
        {:evaluate_code, code, {container_ref, evaluation_ref}, base_locator, opts},
        state
      ) do
    state = ensure_evaluator(state, container_ref)

    base_evaluation_ref =
      case base_locator do
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
      base_evaluation_ref,
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

  def handle_cast({:handle_intellisense, send_to, ref, request, base_locator}, state) do
    {container_ref, evaluation_ref} = base_locator
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

  def handle_cast({:start_smart_cell, kind, ref, attrs, base_locator}, state) do
    definition = Enum.find(state.smart_cell_definitions, &(&1.kind == kind))

    state =
      case DynamicSupervisor.start_child(
             state.smart_cell_supervisor,
             {definition.module, %{ref: ref, attrs: attrs, target_pid: state.owner}}
           ) do
        {:ok, pid, info} ->
          %{js_view: js_view, source: source, scan_binding: scan_binding} = info

          send(
            state.owner,
            {:runtime_smart_cell_started, ref, %{js_view: js_view, source: source}}
          )

          info = %{
            pid: pid,
            scan_binding: scan_binding,
            base_locator: base_locator,
            scan_binding_pending: false,
            scan_binding_monitor_ref: nil
          }

          info = scan_binding_async(ref, info, state)
          put_in(state.smart_cells[ref], info)

        _ ->
          state
      end

    {:noreply, state}
  end

  def handle_cast({:set_smart_cell_base_locator, ref, base_locator}, state) do
    state =
      update_in(state.smart_cells[ref], fn
        %{base_locator: ^base_locator} = info -> info
        info -> scan_binding_async(ref, %{info | base_locator: base_locator}, state)
      end)

    {:noreply, state}
  end

  def handle_cast({:stop_smart_cell, ref}, state) do
    {%{pid: pid}, state} = pop_in(state.smart_cells[ref])

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

  defp report_smart_cell_definitions(state) do
    smart_cell_definitions = get_smart_cell_definitions(state.smart_cell_definitions_module)

    if smart_cell_definitions == state.smart_cell_definitions do
      state
    else
      defs = Enum.map(smart_cell_definitions, &Map.take(&1, [:kind, :name]))
      send(state.owner, {:runtime_smart_cell_definitions, defs})
      %{state | smart_cell_definitions: smart_cell_definitions}
    end
  end

  defp get_smart_cell_definitions(module) do
    if Code.ensure_loaded?(module) and function_exported?(module, :definitions, 0) do
      module.definitions()
    else
      []
    end
  end

  defp scan_binding_async(_ref, %{scan_binding: nil} = info, _state), do: info

  # We wait for the current scanning to finish, this way we avoid
  # race conditions and don't unnecessarily spam evaluators
  defp scan_binding_async(_ref, %{scan_binding_monitor_ref: ref} = info, _state) when ref != nil,
    do: %{info | scan_binding_pending: true}

  defp scan_binding_async(ref, info, state) do
    %{pid: pid, scan_binding: scan_binding} = info

    myself = self()

    scan_and_ack = fn binding, env ->
      try do
        scan_binding.(pid, binding, env)
      rescue
        error -> Logger.error("scanning binding raised an error: #{inspect(error)}")
      end

      send(myself, {:scan_binding_ack, ref})
    end

    {container_ref, evaluation_ref} = info.base_locator
    evaluator = state.evaluators[container_ref]

    worker_pid =
      if evaluator do
        Evaluator.peek_context(evaluator, evaluation_ref, &scan_and_ack.(&1.binding, &1.env))
        evaluator.pid
      else
        {:ok, pid} =
          Task.Supervisor.start_child(state.task_supervisor, fn ->
            binding = []
            # TODO: Use Code.env_for_eval and eval_quoted_with_env on Elixir v1.14+
            env = :elixir.env_for_eval([])
            scan_and_ack.(binding, env)
          end)

        pid
      end

    monitor_ref = Process.monitor(worker_pid)

    %{info | scan_binding_pending: false, scan_binding_monitor_ref: monitor_ref}
  end

  defp finish_scan_binding(ref, state) do
    update_in(state.smart_cells[ref], fn info ->
      Process.demonitor(info.scan_binding_monitor_ref, [:flush])
      info = %{info | scan_binding_monitor_ref: nil}

      if info.scan_binding_pending do
        scan_binding_async(ref, info, state)
      else
        info
      end
    end)
  end

  defp scan_binding_after_evaluation(state, pid, evaluation_ref) do
    {container_ref, _} =
      Enum.find(state.evaluators, fn {_container_ref, evaluator} -> evaluator.pid == pid end)

    locator = {container_ref, evaluation_ref}

    update_in(state.smart_cells, fn smart_cells ->
      Map.map(smart_cells, fn
        {ref, %{base_locator: ^locator} = info} -> scan_binding_async(ref, info, state)
        {_, info} -> info
      end)
    end)
  end
end
