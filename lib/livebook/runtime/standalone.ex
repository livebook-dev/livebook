defmodule Livebook.Runtime.Standalone do
  defstruct [:erl_flags, :node, :server_pid]

  # A runtime backed by a standalone Elixir node managed by Livebook.
  #
  # Livebook is responsible for starting and terminating the node.
  # Most importantly we have to make sure the started node doesn't
  # stay in the system when the session or the entire Livebook
  # terminates.
  #
  # Note: this runtime requires `elixir` executable to be available in
  # the system.
  #
  # ## Connecting
  #
  # Connecting the runtime starts a new Elixir node (a system process).
  # That child node connects back to the parent and notifies that it
  # is ready by sending a `:node_started` message. Next, the parent
  # initializes the child node by loading the necessary modules and
  # starting processes, in particular the node manager and one runtime
  # server. Once done, the parent sends a `:node_initialized` message
  # to the child, and the child starts monitoring the node manager.
  # Once the node manager terminates, the node shuts down.
  #
  # If no process calls `Livebook.Runtime.take_ownership/1` for a
  # period of time, the node automatically terminates. Whoever takes
  # the ownership, becomes the owner and as soon as it terminates,
  # the node shuts down. The node may also be shut down by calling
  # `Livebook.Runtime.disconnect/1`.

  alias Livebook.Utils

  @type t :: %__MODULE__{
          erl_flags: String.t() | nil,
          node: node() | nil,
          server_pid: pid() | nil
        }

  @doc """
  Returns a new runtime instance.

  ## Options

    * `:erl_flags` - erl flags to specify when starting the node

  """
  @spec new(keyword()) :: t()
  def new(opts \\ []) do
    opts = Keyword.validate!(opts, [:erl_flags])
    %__MODULE__{erl_flags: opts[:erl_flags]}
  end

  def __connect__(runtime) do
    caller = self()

    {:ok, pid} =
      DynamicSupervisor.start_child(
        Livebook.RuntimeSupervisor,
        {Task, fn -> do_connect(runtime, caller) end}
      )

    pid
  end

  defp do_connect(runtime, caller) do
    child_node = Livebook.EPMD.random_child_node()

    Utils.temporarily_register(self(), child_node, fn ->
      init_opts = [
        runtime_server_opts: [
          extra_smart_cell_definitions: Livebook.Runtime.Definitions.smart_cell_definitions()
        ]
      ]

      with {:ok, elixir_path} <- find_elixir_executable(),
           port = start_elixir_node(elixir_path, child_node, runtime.erl_flags),
           {:ok, server_pid} <- parent_init_sequence(child_node, port, init_opts) do
        runtime = %{runtime | node: child_node, server_pid: server_pid}
        send(caller, {:runtime_connect_done, self(), {:ok, runtime}})
      else
        {:error, error} ->
          send(caller, {:runtime_connect_done, self(), {:error, error}})
      end
    end)
  end

  defp find_elixir_executable() do
    case System.find_executable("elixir") do
      nil -> {:error, "no Elixir executable found in PATH"}
      path -> {:ok, path}
    end
  end

  defp start_elixir_node(elixir_path, node_name, erl_flags) do
    # Here we create a port to start the system process in a non-blocking way.
    Port.open({:spawn_executable, elixir_path}, [
      :binary,
      # We don't communicate with the system process via stdio,
      # contrarily, we want any non-captured output to go directly
      # to the terminal
      :nouse_stdio,
      :hide,
      args: elixir_flags(node_name, erl_flags)
    ])
  end

  defp parent_init_sequence(child_node, port, init_opts) do
    port_ref = Port.monitor(port)

    loop = fn loop ->
      # Note that the child node terminates when communication times out,
      # so we should always receive either a message or :DOWN event.
      receive do
        {:node_started, init_ref, ^child_node, child_port, primary_pid} ->
          Port.demonitor(port_ref)
          Livebook.EPMD.update_child_node(child_node, child_port)
          server_pid = Livebook.Runtime.ErlDist.initialize(child_node, init_opts)

          send(primary_pid, {:node_initialized, init_ref})
          {:ok, server_pid}

        {^port, {:data, _output}} ->
          loop.(loop)

        {:DOWN, ^port_ref, :port, _object, reason} ->
          {:error,
           "standalone runtime node (#{inspect(child_node)}) terminated unexpectedly on startup, " <>
             "please check your logs for errors. Reason: #{inspect(reason)}"}
      end
    end

    loop.(loop)
  end

  defp child_node_eval_string(node, parent_node, parent_port) do
    # We pass the child node code as --eval argument. Windows handles
    # escaped quotes and newlines differently from Unix, so to avoid
    # those kind of issues, we encode the string in base 64 and pass
    # as positional argument. Then, we use a simple --eval that decodes
    # and evaluates the string.

    quote do
      node = unquote(node)
      parent_node = unquote(parent_node)
      parent_port = unquote(parent_port)

      # We start distribution here, rather than on node boot, so that
      # -pa takes effect and Livebook.EPMD is available
      {:ok, _} = :net_kernel.start(node, %{name_domain: :longnames})
      Livebook.Runtime.EPMD.register_parent(parent_node, parent_port)
      dist_port = Livebook.Runtime.EPMD.dist_port()

      init_ref = make_ref()
      parent_process = {node(), parent_node}
      send(parent_process, {:node_started, init_ref, node(), dist_port, self()})

      receive do
        {:node_initialized, ^init_ref} ->
          manager_ref = Process.monitor(Livebook.Runtime.ErlDist.NodeManager)

          receive do
            {:DOWN, ^manager_ref, :process, _object, _reason} -> :ok
          end
      after
        10_000 ->
          IO.puts(
            "Error: timeout during initial communication between standalone runtime " <>
              "(node: #{inspect(node())}) and Livebook (node: #{inspect(parent_node)})."
          )

          :timeout
      end

      # We explicitly halt at the end, just in case `System.no_halt(true)`
      # is called within the runtime
      System.halt()
    end
    |> Macro.to_string()
    |> Base.encode64()
  end

  defp elixir_flags(node_name, erl_flags) do
    parent_name = node()
    parent_port = Livebook.EPMD.dist_port()

    [
      "--erl",
      # Note: keep these flags in sync with the remote runtime.
      #
      #   * minimize schedulers busy wait threshold, so that they go
      #     to sleep immediately after evaluation
      #
      #   * increase the default stack for dirty IO threads, necessary
      #     for CUDA
      #
      #   * enable ANSI escape codes as we handle them with HTML
      #
      #   * disable stdin, so that the system process never tries to
      #     read terminal input
      #
      #   * specify a custom EPMD module and disable automatic EPMD
      #     startup
      #
      "+sbwt none +sbwtdcpu none +sbwtdio none +sssdio 128 -elixir ansi_enabled true -noinput " <>
        "-epmd_module Elixir.Livebook.Runtime.EPMD " <>
        (erl_flags || ""),
      # Add the location of Livebook.Runtime.EPMD
      "-pa",
      epmd_module_path!(),
      # Make the node hidden, so it doesn't automatically join the cluster
      "--hidden",
      # Use the cookie in Livebook
      "--cookie",
      Atom.to_string(Node.get_cookie()),
      "--eval",
      "System.argv() |> hd() |> Base.decode64!() |> Code.eval_string()",
      child_node_eval_string(node_name, parent_name, parent_port)
    ]
  end

  defp epmd_module_path!() do
    # We need to make the custom Livebook.Runtime.EPMD module available
    # before the child node starts distrubtion. We persist the module
    # into a temporary directory and add to the code paths. Note that
    # we could persist it to priv/ at build time, however for Escript
    # priv/ is packaged into the archive, so it is not accessible in
    # the file system.

    epmd_path = Path.join(Livebook.Config.tmp_path(), "epmd")
    File.rm_rf!(epmd_path)
    File.mkdir_p!(epmd_path)
    {_module, binary, path} = :code.get_object_code(Livebook.Runtime.EPMD)
    File.write!(Path.join(epmd_path, Path.basename(path)), binary)
    epmd_path
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Standalone do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(runtime) do
    [{"Type", "Standalone"}] ++
      if runtime.node do
        [{"Node name", Atom.to_string(runtime.node)}]
      else
        []
      end
  end

  def connect(runtime) do
    Livebook.Runtime.Standalone.__connect__(runtime)
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    :ok = RuntimeServer.stop(runtime.server_pid)
  end

  def duplicate(runtime) do
    Livebook.Runtime.Standalone.new(erl_flags: runtime.erl_flags)
  end

  def evaluate_code(runtime, language, code, locator, parent_locators, opts \\ []) do
    RuntimeServer.evaluate_code(
      runtime.server_pid,
      language,
      code,
      locator,
      parent_locators,
      opts
    )
  end

  def forget_evaluation(runtime, locator) do
    RuntimeServer.forget_evaluation(runtime.server_pid, locator)
  end

  def drop_container(runtime, container_ref) do
    RuntimeServer.drop_container(runtime.server_pid, container_ref)
  end

  def handle_intellisense(runtime, send_to, request, parent_locators, node) do
    RuntimeServer.handle_intellisense(runtime.server_pid, send_to, request, parent_locators, node)
  end

  def read_file(runtime, path) do
    RuntimeServer.read_file(runtime.server_pid, path)
  end

  def transfer_file(runtime, path, file_id, callback) do
    RuntimeServer.transfer_file(runtime.server_pid, path, file_id, callback)
  end

  def relabel_file(runtime, file_id, new_file_id) do
    RuntimeServer.relabel_file(runtime.server_pid, file_id, new_file_id)
  end

  def revoke_file(runtime, file_id) do
    RuntimeServer.revoke_file(runtime.server_pid, file_id)
  end

  def start_smart_cell(runtime, kind, ref, attrs, parent_locators) do
    RuntimeServer.start_smart_cell(runtime.server_pid, kind, ref, attrs, parent_locators)
  end

  def set_smart_cell_parent_locators(runtime, ref, parent_locators) do
    RuntimeServer.set_smart_cell_parent_locators(runtime.server_pid, ref, parent_locators)
  end

  def stop_smart_cell(runtime, ref) do
    RuntimeServer.stop_smart_cell(runtime.server_pid, ref)
  end

  def fixed_dependencies?(_runtime), do: false

  def add_dependencies(_runtime, code, dependencies) do
    Livebook.Runtime.Dependencies.add_dependencies(code, dependencies)
  end

  def has_dependencies?(runtime, dependencies) do
    RuntimeServer.has_dependencies?(runtime.server_pid, dependencies)
  end

  def snippet_definitions(_runtime) do
    Livebook.Runtime.Definitions.snippet_definitions()
  end

  def search_packages(_runtime, send_to, search) do
    Livebook.Runtime.Dependencies.search_packages_on_hex(send_to, search)
  end

  def put_system_envs(runtime, envs) do
    RuntimeServer.put_system_envs(runtime.server_pid, envs)
  end

  def delete_system_envs(runtime, names) do
    RuntimeServer.delete_system_envs(runtime.server_pid, names)
  end

  def restore_transient_state(runtime, transient_state) do
    RuntimeServer.restore_transient_state(runtime.server_pid, transient_state)
  end

  def register_clients(runtime, clients) do
    RuntimeServer.register_clients(runtime.server_pid, clients)
  end

  def unregister_clients(runtime, client_ids) do
    RuntimeServer.unregister_clients(runtime.server_pid, client_ids)
  end

  def fetch_proxy_handler_spec(runtime) do
    RuntimeServer.fetch_proxy_handler_spec(runtime.server_pid)
  end

  def disconnect_node(runtime, node) do
    RuntimeServer.disconnect_node(runtime.server_pid, node)
  end
end
