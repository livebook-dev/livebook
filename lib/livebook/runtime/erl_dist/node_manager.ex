defmodule Livebook.Runtime.ErlDist.NodeManager do
  @moduledoc false

  # The primary Livebook process started on a remote node.
  #
  # This process is responsible for initializing the node
  # with necessary runtime configuration and then starting
  # runtime server processes, one per runtime.
  # This approach allows for multiple runtimes connected
  # to the same node, while preserving the necessary
  # cleanup semantics.
  #
  # The manager process terminates as soon as the last runtime
  # server terminates. Upon termination the manager reverts the
  # runtime configuration back to the initial state.

  use GenServer

  alias Livebook.Runtime.ErlDist

  @name __MODULE__

  @doc """
  Starts the node manager.

  ## Options

    * `:unload_modules_on_termination` - whether to unload all
      Livebook related modules from the node on termination.
      Defaults to `true`.

    * `:anonymous` - configures whether manager should
      be registered under a global name or not.
      In most cases we enforce a single manager per node
      and identify it by a name, but this can be opted-out
      from by using this option. Defaults to `false`.

    * `:auto_termination` - whether to terminate the manager
      when the last runtime server terminates. Defaults to `true`.

    * `:parent_node` - indicates which node spawned the node manager.
       It is used to disconnect the node when the server terminates,
       which happens when the last session using the node disconnects.
       Defaults to `nil`
  """
  def start(opts \\ []) do
    {opts, gen_opts} = split_opts(opts)
    GenServer.start(__MODULE__, opts, gen_opts)
  end

  @doc """
  Starts the node manager with link.

  See `start/1` for available options.
  """
  def start_link(opts \\ []) do
    {opts, gen_opts} = split_opts(opts)
    GenServer.start_link(__MODULE__, opts, gen_opts)
  end

  defp split_opts(opts) do
    {anonymous?, opts} = Keyword.pop(opts, :anonymous, false)

    gen_opts = [
      name: if(anonymous?, do: nil, else: @name)
    ]

    {opts, gen_opts}
  end

  @doc """
  Starts a new `Livebook.Runtime.ErlDist.RuntimeServer` for evaluation.
  """
  @spec start_runtime_server(node() | pid()) :: pid()
  def start_runtime_server(node_or_pid) do
    GenServer.call(server(node_or_pid), :start_runtime_server)
  end

  defp server(pid) when is_pid(pid), do: pid
  defp server(node) when is_atom(node), do: {@name, node}

  @impl true
  def init(opts) do
    unload_modules_on_termination = Keyword.get(opts, :unload_modules_on_termination, true)
    auto_termination = Keyword.get(opts, :auto_termination, true)
    parent_node = Keyword.get(opts, :parent_node)

    ## Initialize the node

    Process.flag(:trap_exit, true)

    {:ok, server_supevisor} = DynamicSupervisor.start_link(strategy: :one_for_one)

    # Register our own standard error IO device that proxies
    # to sender's group leader.
    original_standard_error = Process.whereis(:standard_error)
    {:ok, io_forward_gl_pid} = ErlDist.IOForwardGL.start_link()
    Process.unregister(:standard_error)
    Process.register(io_forward_gl_pid, :standard_error)

    Logger.add_backend(Livebook.Runtime.ErlDist.LoggerGLBackend)

    # Set `ignore_module_conflict` only for the NodeManager lifetime.
    initial_ignore_module_conflict = Code.compiler_options()[:ignore_module_conflict]
    Code.compiler_options(ignore_module_conflict: true)

    {:ok,
     %{
       unload_modules_on_termination: unload_modules_on_termination,
       auto_termination: auto_termination,
       server_supevisor: server_supevisor,
       runtime_servers: [],
       initial_ignore_module_conflict: initial_ignore_module_conflict,
       original_standard_error: original_standard_error,
       parent_node: parent_node
     }}
  end

  @impl true
  def terminate(_reason, state) do
    Code.compiler_options(ignore_module_conflict: state.initial_ignore_module_conflict)

    Process.unregister(:standard_error)
    Process.register(state.original_standard_error, :standard_error)

    Logger.remove_backend(Livebook.Runtime.ErlDist.LoggerGLBackend)

    if state.unload_modules_on_termination do
      ErlDist.unload_required_modules()
    end

    if state.parent_node do
      Node.disconnect(state.parent_node)
    end

    :ok
  end

  @impl true
  def handle_info({:DOWN, _, :process, pid, _}, state) do
    if pid in state.runtime_servers do
      case update_in(state.runtime_servers, &List.delete(&1, pid)) do
        %{runtime_servers: [], auto_termination: true} = state ->
          {:stop, :shutdown, state}

        state ->
          {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end

  def handle_info(_message, state), do: {:noreply, state}

  @impl true
  def handle_call(:start_runtime_server, _from, state) do
    {:ok, server_pid} =
      DynamicSupervisor.start_child(state.server_supevisor, ErlDist.RuntimeServer)

    Process.monitor(server_pid)
    state = update_in(state.runtime_servers, &[server_pid | &1])
    {:reply, server_pid, state}
  end
end
