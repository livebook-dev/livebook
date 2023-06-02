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
  Starts the runtime server.

  Note: make sure to call `attach` within #{@await_owner_timeout}ms
  or the runtime server assumes it's not needed and terminates.

  ## Options

    * `:smart_cell_definitions_module` - the module to read smart
      cell definitions from, it needs to export a `definitions/0`
      function. Defaults to `Kino.SmartCell`

    * `:extra_smart_cell_definitions` - a list of predefined smart
      cell definitions, that may be currently be unavailable, but
      should be reported together with their requirements

    * `:ebin_path` - a directory to write modules bytecode into. When
      not specified, modules are not written to disk

    * `:tmp_dir` - a temporary directory to write files into, such as
      those from file inputs. When not specified, operations relying
      on the directory are not possible

    * `:base_path_env` - the value of `PATH` environment variable
      to merge new values into when setting environment variables.
      Defaults to `System.get_env("PATH", "")`

    * `:io_proxy_registry` - the registry to register IO proxy
      processes in

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

  See `Livebook.Runtime.take_ownership/2` for the list of available
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
  @spec evaluate_code(
          pid(),
          :elixir | :erlang,
          String.t(),
          Runtime.locator(),
          Runtime.parent_locators(),
          keyword()
        ) :: :ok
  def evaluate_code(pid, language, code, locator, parent_locators, opts \\ []) do
    GenServer.cast(pid, {:evaluate_code, language, code, locator, parent_locators, opts})
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
          Runtime.intellisense_request(),
          Runtime.Runtime.parent_locators()
        ) :: reference()
  def handle_intellisense(pid, send_to, request, parent_locators) do
    ref = make_ref()
    GenServer.cast(pid, {:handle_intellisense, send_to, ref, request, parent_locators})
    ref
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
  Transfers file at `path` to the runtime.

  If the runtime is at the same host as the caller, no copying occurs.

  See `Livebook.Runtime` for more details.
  """
  @spec transfer_file(pid(), String.t(), String.t(), (path :: String.t() | nil -> any())) :: :ok
  def transfer_file(pid, path, file_id, callback) do
    if same_host?(pid) do
      callback.(path)
    else
      Task.Supervisor.start_child(Livebook.TaskSupervisor, fn ->
        md5 = file_md5(path)

        target_path =
          case GenServer.call(pid, {:transfer_file_open, file_id, md5}, :infinity) do
            {:noop, target_path} ->
              target_path

            {:transfer, target_path, target_pid} ->
              try do
                path
                |> File.stream!([], 2048)
                |> Enum.each(fn chunk -> IO.binwrite(target_pid, chunk) end)

                target_path
              rescue
                _error -> nil
              after
                File.close(target_pid)
              end
          end

        callback.(target_path)
      end)
    end

    :ok
  end

  @doc """
  Removes the file created by `transfer_file/4`, if any.

  See `Livebook.Runtime` for more details.
  """
  @spec revoke_file(pid(), String.t()) :: :ok
  def revoke_file(pid, file_id) do
    unless same_host?(pid) do
      GenServer.cast(pid, {:revoke_file, file_id})
    end

    :ok
  end

  @doc """
  Starts a new smart cell.
  """
  @spec start_smart_cell(
          pid(),
          String.t(),
          Runtime.smart_cell_ref(),
          Runtime.smart_cell_attrs(),
          Runtime.Runtime.parent_locators()
        ) :: :ok
  def start_smart_cell(pid, kind, ref, attrs, parent_locators) do
    GenServer.cast(pid, {:start_smart_cell, kind, ref, attrs, parent_locators})
  end

  @doc """
  Updates the parent locator used by a smart cell as its context.
  """
  @spec set_smart_cell_parent_locators(
          pid(),
          Runtime.smart_cell_ref(),
          Runtime.Runtime.parent_locators()
        ) :: :ok
  def set_smart_cell_parent_locators(pid, ref, parent_locators) do
    GenServer.cast(pid, {:set_smart_cell_parent_locators, ref, parent_locators})
  end

  @doc """
  Stops the given smart cell.
  """
  @spec stop_smart_cell(pid(), String.t()) :: :ok
  def stop_smart_cell(pid, ref) do
    GenServer.cast(pid, {:stop_smart_cell, ref})
  end

  @doc """
  Checks if the given dependencies are already installed within the
  runtime.
  """
  @spec has_dependencies?(pid(), list(Runtime.dependency())) :: boolean()
  def has_dependencies?(pid, dependencies) do
    GenServer.call(pid, {:has_dependencies?, dependencies})
  end

  @doc """
  Disables dependencies cache globally.
  """
  @spec disable_dependencies_cache(pid()) :: :ok
  def disable_dependencies_cache(pid) do
    GenServer.cast(pid, :disable_dependencies_cache)
  end

  @doc """
  Sets the given environment variables.
  """
  @spec put_system_envs(pid(), list({String.t(), String.t()})) :: :ok
  def put_system_envs(pid, envs) do
    GenServer.cast(pid, {:put_system_envs, envs})
  end

  @doc """
  Unsets the given environment variables.
  """
  @spec delete_system_envs(pid(), list(String.t())) :: :ok
  def delete_system_envs(pid, names) do
    GenServer.cast(pid, {:delete_system_envs, names})
  end

  @doc """
  Stops the runtime server.

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
       smart_cell_definitions: nil,
       smart_cell_definitions_module:
         Keyword.get(opts, :smart_cell_definitions_module, Kino.SmartCell),
       extra_smart_cell_definitions: Keyword.get(opts, :extra_smart_cell_definitions, []),
       memory_timer_ref: nil,
       last_evaluator: nil,
       base_env_path:
         Keyword.get_lazy(opts, :base_env_path, fn -> System.get_env("PATH", "") end),
       ebin_path: Keyword.get(opts, :ebin_path),
       io_proxy_registry: Keyword.get(opts, :io_proxy_registry),
       tmp_dir: Keyword.get(opts, :tmp_dir)
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
     |> handle_down_scan_binding(message)
     |> handle_down_smart_cell(message)}
  end

  def handle_info({:evaluation_finished, locator}, state) do
    {:noreply,
     state
     |> report_smart_cell_definitions()
     |> scan_binding_after_evaluation(locator)}
  end

  def handle_info(:memory_usage, state) do
    report_memory_usage(state)
    schedule_memory_usage_report()
    {:noreply, state}
  end

  def handle_info({:scan_binding_ack, ref}, state) do
    {:noreply, finish_scan_binding(ref, state)}
  end

  def handle_info({:orphan_log, output}, state) do
    with %{} = evaluator <- state.last_evaluator,
         {:group_leader, io_proxy} <- Process.info(evaluator.pid, :group_leader) do
      ErlDist.LoggerGLBackend.async_io(io_proxy, output)
    end

    {:noreply, state}
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
    state.smart_cells
    |> Enum.find(fn {_ref, info} -> info.scan_binding_monitor_ref == monitor_ref end)
    |> case do
      {ref, _info} -> finish_scan_binding(ref, state)
      nil -> state
    end
  end

  defp handle_down_smart_cell(state, {:DOWN, monitor_ref, :process, _, _}) do
    state.smart_cells
    |> Enum.find(fn {_ref, info} -> info.monitor_ref == monitor_ref end)
    |> case do
      {ref, _info} ->
        send(state.owner, {:runtime_smart_cell_down, ref})
        {_, state} = pop_in(state.smart_cells[ref])
        state

      nil ->
        state
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
        {:evaluate_code, language, code, {container_ref, evaluation_ref} = locator,
         parent_locators, opts},
        state
      ) do
    state = ensure_evaluator(state, container_ref)

    parent_evaluation_refs = evaluation_refs_for_container(state, container_ref, parent_locators)

    {smart_cell_ref, opts} = Keyword.pop(opts, :smart_cell_ref)
    smart_cell_info = smart_cell_ref && state.smart_cells[smart_cell_ref]

    myself = self()

    opts =
      Keyword.put(opts, :on_finish, fn result ->
        with %{scan_eval_result: scan_eval_result} when scan_eval_result != nil <- smart_cell_info do
          try do
            smart_cell_info.scan_eval_result.(smart_cell_info.pid, result)
          rescue
            error -> Logger.error("scanning evaluation result raised an error: #{inspect(error)}")
          end
        end

        send(myself, {:evaluation_finished, locator})
      end)

    Evaluator.evaluate_code(
      state.evaluators[container_ref],
      language,
      code,
      evaluation_ref,
      parent_evaluation_refs,
      opts
    )

    {:noreply, %{state | last_evaluator: state.evaluators[container_ref]}}
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

  def handle_cast({:handle_intellisense, send_to, ref, request, parent_locators}, state) do
    {container_ref, parent_evaluation_refs} =
      case parent_locators do
        [] ->
          {nil, []}

        [{container_ref, _} | _] ->
          parent_evaluation_refs =
            parent_locators
            # If there is a parent evaluator we ignore it and use whatever
            # initial context we currently have in the evaluator. We sync
            # initial context only on evaluation, since it may be blocking
            |> Enum.take_while(&(elem(&1, 0) == container_ref))
            |> Enum.map(&elem(&1, 1))

          {container_ref, parent_evaluation_refs}
      end

    evaluator = container_ref && state.evaluators[container_ref]

    intellisense_context =
      if evaluator == nil or elem(request, 0) in [:format] do
        Evaluator.intellisense_context()
      else
        Evaluator.intellisense_context(evaluator, parent_evaluation_refs)
      end

    Task.Supervisor.start_child(state.task_supervisor, fn ->
      response = Livebook.Intellisense.handle_request(request, intellisense_context)
      send(send_to, {:runtime_intellisense_response, ref, request, response})
    end)

    {:noreply, state}
  end

  def handle_cast({:start_smart_cell, kind, ref, attrs, parent_locators}, state) do
    definition = Enum.find(state.smart_cell_definitions, &(&1.kind == kind))

    state =
      case DynamicSupervisor.start_child(
             state.smart_cell_supervisor,
             {definition.module, %{ref: ref, attrs: attrs, target_pid: state.owner}}
           ) do
        {:ok, pid, info} ->
          %{
            source: source,
            js_view: js_view,
            editor: editor,
            scan_binding: scan_binding,
            scan_eval_result: scan_eval_result
          } = info

          chunks = info[:chunks]

          send(
            state.owner,
            {:runtime_smart_cell_started, ref,
             %{source: source, chunks: chunks, js_view: js_view, editor: editor}}
          )

          info = %{
            pid: pid,
            monitor_ref: Process.monitor(pid),
            scan_binding: scan_binding,
            parent_locators: parent_locators,
            scan_binding_pending: false,
            scan_binding_monitor_ref: nil,
            scan_eval_result: scan_eval_result
          }

          info = scan_binding_async(ref, info, state)
          put_in(state.smart_cells[ref], info)

        {:error, error} ->
          Logger.error("failed to start smart cell - #{Exception.format_exit(error)}")
          state
      end

    {:noreply, state}
  end

  def handle_cast({:set_smart_cell_parent_locators, ref, parent_locators}, state) do
    state =
      update_in(state.smart_cells[ref], fn
        %{parent_locators: ^parent_locators} = info -> info
        info -> scan_binding_async(ref, %{info | parent_locators: parent_locators}, state)
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

  def handle_cast(:disable_dependencies_cache, state) do
    System.put_env("MIX_INSTALL_FORCE", "true")

    {:noreply, state}
  end

  def handle_cast({:put_system_envs, envs}, state) do
    envs
    |> Enum.map(fn
      {"PATH", path} -> {"PATH", state.base_env_path <> os_path_separator() <> path}
      other -> other
    end)
    |> System.put_env()

    {:noreply, state}
  end

  def handle_cast({:delete_system_envs, names}, state) do
    names
    |> Enum.map(fn
      "PATH" -> {"PATH", state.base_env_path}
      name -> {name, nil}
    end)
    |> System.put_env()

    {:noreply, state}
  end

  def handle_cast({:revoke_file, file_id}, state) do
    target_path = file_path(state, file_id)
    File.rm(target_path)

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

  def handle_call({:transfer_file_open, file_id, md5}, _from, state) do
    reply =
      if target_path = file_path(state, file_id) do
        current_md5 = if File.exists?(target_path), do: file_md5(target_path)

        if current_md5 == md5 do
          {:noop, target_path}
        else
          target_path |> Path.dirname() |> File.mkdir_p!()
          target_pid = File.open!(target_path, [:binary, :write])
          {:transfer, target_path, target_pid}
        end
      else
        {:noop, nil}
      end

    {:reply, reply, state}
  end

  def handle_call({:has_dependencies?, dependencies}, _from, state) do
    has_dependencies? = Enum.all?(dependencies, &dependency_installed?/1)
    {:reply, has_dependencies?, state}
  end

  defp file_path(state, file_id) do
    if tmp_dir = state.tmp_dir do
      Path.join([tmp_dir, "files", file_id])
    end
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
          object_tracker: state.object_tracker,
          ebin_path: state.ebin_path,
          io_proxy_registry: state.io_proxy_registry
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
      available_defs =
        for definition <- smart_cell_definitions,
            do: %{kind: definition.kind, name: definition.name, requirement_presets: []}

      defs = Enum.uniq_by(available_defs ++ state.extra_smart_cell_definitions, & &1.kind)

      if defs != [] do
        send(state.owner, {:runtime_smart_cell_definitions, defs})
      end

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

    {container_ref, parent_evaluation_refs} =
      case info.parent_locators do
        [] ->
          {nil, []}

        [{container_ref, _} | _] = parent_locators ->
          parent_evaluation_refs =
            evaluation_refs_for_container(state, container_ref, parent_locators)

          {container_ref, parent_evaluation_refs}
      end

    evaluator = container_ref && state.evaluators[container_ref]

    worker_pid =
      if evaluator do
        Evaluator.peek_context(
          evaluator,
          parent_evaluation_refs,
          &scan_and_ack.(&1.binding, &1.env)
        )

        evaluator.pid
      else
        {:ok, pid} =
          Task.Supervisor.start_child(state.task_supervisor, fn ->
            binding = []
            env = Code.env_for_eval([])
            scan_and_ack.(binding, env)
          end)

        pid
      end

    monitor_ref = Process.monitor(worker_pid)

    %{info | scan_binding_pending: false, scan_binding_monitor_ref: monitor_ref}
  end

  defp evaluation_refs_for_container(state, container_ref, locators) do
    case Enum.split_while(locators, &(elem(&1, 0) == container_ref)) do
      {locators, []} ->
        Enum.map(locators, &elem(&1, 1))

      {locators, [{source_container_ref, _} | _] = source_locators} ->
        source_evaluation_refs = Enum.map(source_locators, &elem(&1, 1))

        evaluator = state.evaluators[container_ref]
        source_evaluator = state.evaluators[source_container_ref]

        if evaluator && source_evaluator do
          # Synchronize initial state in the child evaluator
          Evaluator.initialize_from(evaluator, source_evaluator, source_evaluation_refs)
        end

        Enum.map(locators, &elem(&1, 1))
    end
  end

  defp finish_scan_binding(ref, state) do
    if state.smart_cells[ref] do
      update_in(state.smart_cells[ref], fn info ->
        Process.demonitor(info.scan_binding_monitor_ref, [:flush])
        info = %{info | scan_binding_monitor_ref: nil}

        if info.scan_binding_pending do
          scan_binding_async(ref, info, state)
        else
          info
        end
      end)
    else
      state
    end
  end

  defp scan_binding_after_evaluation(state, locator) do
    update_in(state.smart_cells, fn smart_cells ->
      Map.new(smart_cells, fn {ref, info} ->
        if locator in info.parent_locators do
          {ref, scan_binding_async(ref, info, state)}
        else
          {ref, info}
        end
      end)
    end)
  end

  defp os_path_separator() do
    case :os.type() do
      {:win32, _} -> ";"
      _ -> ":"
    end
  end

  defp same_host?(pid) do
    node_host(node()) == node_host(node(pid))
  end

  defp node_host(node) do
    [_nodename, hostname] =
      node
      |> Atom.to_charlist()
      |> :string.split(~c"@")

    hostname
  end

  defp file_md5(path) do
    File.stream!(path, [], 2048)
    |> Enum.reduce(:erlang.md5_init(), &:erlang.md5_update(&2, &1))
    |> :erlang.md5_final()
  end

  defp dependency_installed?(dependency) do
    name = elem(dependency.dep, 0)
    Application.spec(name) != nil
  end
end
