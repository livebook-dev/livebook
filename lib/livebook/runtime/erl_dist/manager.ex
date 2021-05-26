defmodule Livebook.Runtime.ErlDist.Manager do
  @moduledoc false

  # The primary Livebook process started on a remote node.
  #
  # This process is responsible for monitoring the owner
  # process on the main node and cleaning up if it terminates.
  # Also, this process keeps track of the evaluators
  # and spawns/terminates them whenever necessary for the evaluation.

  use GenServer

  alias Livebook.Evaluator
  alias Livebook.Runtime.ErlDist

  @name __MODULE__

  @await_owner_timeout 5_000

  @doc """
  Starts the manager.

  Note: make sure to call `set_owner` within `@await_owner_timeout`
  or the manager assumes it's not needed and terminates.

  ## Options

    * `:anonymous` - configures whether manager should
      be registered under a global name or not.
      In most cases we enforce a single manager per node
      and identify it by a name, but this can be opted-out
      from using this option. Defaults to `false`.

    * `:cleanup_on_termination` - configures whether
      manager should cleanup any global configuration
      it altered and unload Livebook-specific modules
      from the node. Defaults to `true`.

    * `:register_standard_error_proxy` - configures whether
      manager should start an IOForwardGL process and register
      it as `:standard_error`. Defaults to `true`.
  """
  def start(opts \\ []) do
    {anonymous?, opts} = Keyword.pop(opts, :anonymous, false)

    gen_opts = [
      name: if(anonymous?, do: nil, else: @name)
    ]

    GenServer.start(__MODULE__, opts, gen_opts)
  end

  @doc """
  Sets the owner process.

  The owner process is watched and as soon as it terminates,
  the manager also terminates. All the evaluation results are
  send directly to the owner.
  """
  @spec set_owner(node() | pid(), pid()) :: :ok
  def set_owner(node_or_pid, owner) do
    GenServer.cast(server(node_or_pid), {:set_owner, owner})
  end

  @doc """
  Evaluates the given code using an `Evaluator` process
  belonging to the given `container_ref` and instructs
  it to send all the outputs to the owner process.

  If that's the first evaluation for this `container_ref`,
  a new evaluator is started.

  See `Evaluator` for more details.
  """
  @spec evaluate_code(
          node() | pid(),
          String.t(),
          Evaluator.ref(),
          Evaluator.ref(),
          Evaluator.ref() | nil,
          keyword()
        ) :: :ok
  def evaluate_code(
        node_or_pid,
        code,
        container_ref,
        evaluation_ref,
        prev_evaluation_ref,
        opts \\ []
      ) do
    GenServer.cast(
      server(node_or_pid),
      {:evaluate_code, code, container_ref, evaluation_ref, prev_evaluation_ref, opts}
    )
  end

  @doc """
  Removes the specified evaluation from the history.

  See `Evaluator` for more details.
  """
  @spec forget_evaluation(node() | pid(), Evaluator.ref(), Evaluator.ref()) :: :ok
  def forget_evaluation(node_or_pid, container_ref, evaluation_ref) do
    GenServer.cast(server(node_or_pid), {:forget_evaluation, container_ref, evaluation_ref})
  end

  @doc """
  Terminates the `Evaluator` process belonging to the given container.
  """
  @spec drop_container(node() | pid(), Evaluator.ref()) :: :ok
  def drop_container(node_or_pid, container_ref) do
    GenServer.cast(server(node_or_pid), {:drop_container, container_ref})
  end

  @doc """
  Asynchronously sends completion request for the given `hint` text.

  The completion request is forwarded to `Evaluator` process
  belonging to the given `container_ref`. If there's not evaluator,
  there's also no binding and environment, so the completion is handled
  by a temporary process.

  See `Livebook.Runtime` for more details.
  """
  @spec request_completion_items(
          node() | pid(),
          pid(),
          term(),
          String.t(),
          Evaluator.ref(),
          Evaluator.ref()
        ) :: :ok
  def request_completion_items(node_or_pid, send_to, ref, hint, container_ref, evaluation_ref) do
    GenServer.cast(
      server(node_or_pid),
      {:request_completion_items, send_to, ref, hint, container_ref, evaluation_ref}
    )
  end

  @doc """
  Stops the manager.

  This results in all Livebook-related modules being unloaded from this node.
  """
  @spec stop(node() | pid()) :: :ok
  def stop(node_or_pid) do
    GenServer.stop(server(node_or_pid))
  end

  defp server(pid) when is_pid(pid), do: pid
  defp server(node) when is_atom(node), do: {@name, node}

  @impl true
  def init(opts) do
    cleanup_on_termination = Keyword.get(opts, :cleanup_on_termination, true)
    register_standard_error_proxy = Keyword.get(opts, :register_standard_error_proxy, true)

    Process.send_after(self(), :check_owner, @await_owner_timeout)

    ## Initialize the node

    Process.flag(:trap_exit, true)

    {:ok, evaluator_supervisor} = ErlDist.EvaluatorSupervisor.start_link()
    {:ok, completion_supervisor} = Task.Supervisor.start_link()

    # Register our own standard error IO device that proxies
    # to sender's group leader.

    original_standard_error = Process.whereis(:standard_error)

    if register_standard_error_proxy do
      {:ok, io_forward_gl_pid} = ErlDist.IOForwardGL.start_link()

      Process.unregister(:standard_error)
      Process.register(io_forward_gl_pid, :standard_error)
    end

    Logger.add_backend(Livebook.Runtime.ErlDist.LoggerGLBackend)

    # Set `ignore_module_conflict` only for the Manager lifetime.
    initial_ignore_module_conflict = Code.compiler_options()[:ignore_module_conflict]
    Code.compiler_options(ignore_module_conflict: true)

    {:ok,
     %{
       cleanup_on_termination: cleanup_on_termination,
       register_standard_error_proxy: register_standard_error_proxy,
       owner: nil,
       evaluators: %{},
       evaluator_supervisor: evaluator_supervisor,
       completion_supervisor: completion_supervisor,
       initial_ignore_module_conflict: initial_ignore_module_conflict,
       original_standard_error: original_standard_error
     }}
  end

  @impl true
  def terminate(_reason, state) do
    if state.cleanup_on_termination do
      Code.compiler_options(ignore_module_conflict: state.initial_ignore_module_conflict)

      if state.register_standard_error_proxy do
        Process.unregister(:standard_error)
        Process.register(state.original_standard_error, :standard_error)
      end

      Logger.remove_backend(Livebook.Runtime.ErlDist.LoggerGLBackend)

      ErlDist.unload_required_modules()
    end

    :ok
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
    {:stop, :normal, state}
  end

  def handle_info({:DOWN, _, :process, pid, reason}, state) do
    state.evaluators
    |> Enum.find(fn {_container_ref, evaluator_pid} ->
      evaluator_pid == pid
    end)
    |> case do
      {container_ref, _} ->
        message = Exception.format_exit(reason)
        send(state.owner, {:container_down, container_ref, message})
        {:noreply, %{state | evaluators: Map.delete(state.evaluators, container_ref)}}

      nil ->
        {:noreply, state}
    end
  end

  def handle_info({:EXIT, _from, _reason}, state) do
    {:stop, :shutdown, state}
  end

  def handle_info(_message, state), do: {:noreply, state}

  @impl true
  def handle_cast({:set_owner, owner}, state) do
    Process.monitor(owner)

    {:noreply, %{state | owner: owner}}
  end

  def handle_cast(
        {:evaluate_code, code, container_ref, evaluation_ref, prev_evaluation_ref, opts},
        state
      ) do
    state = ensure_evaluator(state, container_ref)

    Evaluator.evaluate_code(
      state.evaluators[container_ref],
      state.owner,
      code,
      evaluation_ref,
      prev_evaluation_ref,
      opts
    )

    {:noreply, state}
  end

  def handle_cast({:forget_evaluation, container_ref, evaluation_ref}, state) do
    with {:ok, evaluator} <- Map.fetch(state.evaluators, container_ref) do
      Evaluator.forget_evaluation(evaluator, evaluation_ref)
    end

    {:noreply, state}
  end

  def handle_cast({:drop_container, container_ref}, state) do
    state = discard_evaluator(state, container_ref)
    {:noreply, state}
  end

  def handle_cast(
        {:request_completion_items, send_to, ref, hint, container_ref, evaluation_ref},
        state
      ) do
    if evaluator = Map.get(state.evaluators, container_ref) do
      Evaluator.request_completion_items(evaluator, send_to, ref, hint, evaluation_ref)
    else
      # Since there's no evaluator, we may as well get the completion items here.
      Task.Supervisor.start_child(state.completion_supervisor, fn ->
        binding = []
        env = :elixir.env_for_eval([])
        items = Livebook.Completion.get_completion_items(hint, binding, env)
        send(send_to, {:completion_response, ref, items})
      end)
    end

    {:noreply, state}
  end

  defp ensure_evaluator(state, container_ref) do
    if Map.has_key?(state.evaluators, container_ref) do
      state
    else
      {:ok, evaluator} = ErlDist.EvaluatorSupervisor.start_evaluator(state.evaluator_supervisor)
      Process.monitor(evaluator)
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
end
