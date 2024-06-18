defmodule Livebook.Runtime.ElixirStandalone do
  defstruct [:node, :server_pid]

  # A runtime backed by a standalone Elixir node managed by Livebook.
  #
  # Livebook is responsible for starting and terminating the node.
  # Most importantly we have to make sure the started node doesn't
  # stay in the system when the session or the entire Livebook
  # terminates.

  alias Livebook.Utils

  @type t :: %__MODULE__{
          node: node() | nil,
          server_pid: pid() | nil
        }

  @doc """
  Returns a new runtime instance.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{}
  end

  @doc """
  Starts a new Elixir node (a system process) and initializes it with
  Livebook-specific modules and processes.

  If no process calls `Runtime.take_ownership/1` for a period of time,
  the node automatically terminates. Whoever takes the ownersihp,
  becomes the owner and as soon as it terminates, the node terminates
  as well. The node may also be terminated by calling `Runtime.disconnect/1`.

  Note: to start the node it is required that `elixir` is a recognised
  executable within the system.
  """
  @spec connect(t()) :: {:ok, t()} | {:error, String.t()}
  def connect(runtime) do
    child_node = Livebook.EPMD.random_child_node()

    Utils.temporarily_register(self(), child_node, fn ->
      init_opts = [
        runtime_server_opts: [
          extra_smart_cell_definitions: Livebook.Runtime.Definitions.smart_cell_definitions()
        ]
      ]

      with {:ok, elixir_path} <- find_elixir_executable(),
           port = start_elixir_node(elixir_path, child_node),
           {:ok, server_pid} <- parent_init_sequence(child_node, port, init_opts) do
        runtime = %{runtime | node: child_node, server_pid: server_pid}
        {:ok, runtime}
      else
        {:error, error} -> {:error, error}
      end
    end)
  end

  defp find_elixir_executable() do
    case System.find_executable("elixir") do
      nil -> {:error, "no Elixir executable found in PATH"}
      path -> {:ok, path}
    end
  end

  defp start_elixir_node(elixir_path, node_name) do
    # Here we create a port to start the system process in a non-blocking way.
    Port.open({:spawn_executable, elixir_path}, [
      :binary,
      # We don't communicate with the system process via stdio,
      # contrarily, we want any non-captured output to go directly
      # to the terminal
      :nouse_stdio,
      :hide,
      args: elixir_flags(node_name)
    ])
  end

  # ---
  #
  # Once the new node is spawned we need to establish a connection,
  # initialize it and make sure it correctly reacts to the parent node terminating.
  #
  # The procedure goes as follows:
  #
  # 1. The child sends {:node_initialized, ref} message to the parent
  #    to communicate it's ready for initialization.
  #
  # 2. The parent initializes the child node - loads necessary modules,
  #    starts the NodeManager process and a single RuntimeServer process.
  #
  # 3. The parent sends {:node_initialized, ref} message back to the child,
  #    to communicate successful initialization.
  #
  # 4. The child starts monitoring the NodeManager process and freezes
  #    until the NodeManager process terminates. The NodeManager process
  #    serves as the leading remote process and represents the node from now on.
  #
  # The nodes either successfully go through this flow or return an error,
  # either if the other node dies or is not responding for too long.
  #
  # ---

  defp parent_init_sequence(child_node, port, init_opts) do
    port_ref = Port.monitor(port)

    loop = fn loop ->
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
           "Elixir terminated unexpectedly, please check your logs for errors. Reason: #{inspect(reason)}"}
      after
        # Use a longer timeout to account for longer child node startup.
        30_000 ->
          {:error, "connection timed out"}
      end
    end

    loop.(loop)
  end

  # Note Windows does not handle escaped quotes and newlines the same way as Unix,
  # so the string cannot have constructs newlines nor strings. That's why we pass
  # the parent node name as ARGV and write the code avoiding newlines.
  #
  # This boot script must be kept in sync with Livebook.EPMD.
  #
  # Also note that we explicitly halt, just in case `System.no_halt(true)` is
  # called within the runtime.
  @child_node_eval_string """
  {:ok, [[node]]} = :init.get_argument(:livebook_current);\
  {:ok, _} = :net_kernel.start(List.to_atom(node), %{name_domain: :longnames});\
  {:ok, [[parent_node, _port]]} = :init.get_argument(:livebook_parent);\
  dist_port = :persistent_term.get(:livebook_dist_port, 0);\
  init_ref = make_ref();\
  parent_process = {node(), List.to_atom(parent_node)};\
  send(parent_process, {:node_started, init_ref, node(), dist_port, self()});\
  receive do {:node_initialized, ^init_ref} ->\
    manager_ref = Process.monitor(Livebook.Runtime.ErlDist.NodeManager);\
    receive do {:DOWN, ^manager_ref, :process, _object, _reason} -> :ok end;\
  after 10_000 ->\
    :timeout;\
  end;\
  System.halt()\
  """

  if @child_node_eval_string =~ "\n" do
    raise "invalid @child_node_eval_string, newline found: #{inspect(@child_node_eval_string)}"
  end

  defp elixir_flags(node_name) do
    parent_name = node()
    parent_port = Livebook.EPMD.dist_port()

    epmdless_flags =
      if parent_port != 0 do
        "-epmd_module Elixir.Livebook.EPMD -start_epmd false -erl_epmd_port 0 "
      else
        ""
      end

    [
      "--erl",
      # Minimize schedulers busy wait threshold,
      # so that they go to sleep immediately after evaluation.
      # Increase the default stack for dirty io threads (cuda requires it).
      # Enable ANSI escape codes as we handle them with HTML.
      # Disable stdin, so that the system process never tries to read terminal input.
      "+sbwt none +sbwtdcpu none +sbwtdio none +sssdio 128 -elixir ansi_enabled true -noinput " <>
        epmdless_flags <>
        "-livebook_parent #{parent_name} #{parent_port} -livebook_current #{node_name}",
      # Add the location of Livebook.EPMD
      "-pa",
      Application.app_dir(:livebook, "priv/epmd"),
      # Make the node hidden, so it doesn't automatically join the cluster
      "--hidden",
      # Use the cookie in Livebook
      "--cookie",
      Atom.to_string(Node.get_cookie()),
      "--eval",
      @child_node_eval_string
    ]
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.ElixirStandalone do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(runtime) do
    [{"Type", "Elixir standalone"}] ++
      if connected?(runtime) do
        [{"Node name", Atom.to_string(runtime.node)}]
      else
        []
      end
  end

  def connect(runtime) do
    Livebook.Runtime.ElixirStandalone.connect(runtime)
  end

  def connected?(runtime) do
    runtime.server_pid != nil
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    :ok = RuntimeServer.stop(runtime.server_pid)
    {:ok, %{runtime | node: nil, server_pid: nil}}
  end

  def duplicate(_runtime) do
    Livebook.Runtime.ElixirStandalone.new()
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

  def disable_dependencies_cache(runtime) do
    RuntimeServer.disable_dependencies_cache(runtime.server_pid)
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
