defmodule Livebook.Runtime.Attached do
  # A runtime backed by an Elixir node managed externally.
  #
  # Such node must be already started and accessible. Livebook doesn't
  # manage the node's lifetime in any way and only loads/unloads the
  # necessary modules and processes. The node can be an ordinary Elixir
  # runtime, a Mix project shell, a running release or anything else.

  defstruct [:node, :cookie, :server_pid]

  @type t :: %__MODULE__{
          node: node(),
          cookie: atom(),
          server_pid: pid() | nil
        }

  @doc """
  Returns a new runtime instance.
  """
  @spec new(node(), atom()) :: t()
  def new(node, cookie \\ Node.get_cookie()) do
    %__MODULE__{node: node, cookie: cookie}
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
    %{node: node, cookie: cookie} = runtime

    # Set cookie for connecting to this specific node
    Node.set_cookie(node, cookie)

    with :ok <- connect_to_node(node),
         :ok <- check_attached_node_version(node) do
      server_pid =
        Livebook.Runtime.ErlDist.initialize(node,
          node_manager_opts: [parent_node: node(), capture_orphan_logs: false]
        )

      runtime = %{runtime | node: node, server_pid: server_pid}
      send(caller, {:runtime_connect_done, self(), {:ok, runtime}})
    else
      {:error, error} ->
        send(caller, {:runtime_connect_done, self(), {:error, error}})
    end
  end

  defp connect_to_node(node) do
    with true <- :net_kernel.hidden_connect_node(node),
         :pong <- Node.ping(node) do
      :ok
    else
      _ -> {:error, "node #{inspect(node)} is unreachable"}
    end
  end

  defp check_attached_node_version(node) do
    attached_node_version = :erpc.call(node, System, :version, [])

    requirement = elixir_version_requirement()

    if Version.match?(attached_node_version, requirement) do
      :ok
    else
      {:error, "the node uses Elixir #{attached_node_version}, but #{requirement} is required"}
    end
  end

  @elixir_version_requirement Keyword.fetch!(Mix.Project.config(), :elixir)

  @doc """
  Returns requirement for the attached node Elixir version.
  """
  @spec elixir_version_requirement() :: String.t()
  def elixir_version_requirement() do
    # We load compiled modules binary into the remote node. Erlang
    # provides rather good compatibility of the binary format, and
    # in case loading fails we show an appropriate message. However,
    # it is more likely that the Elixir core functions used in the
    # compiled module differ across versions. We assume that such
    # changes are unlikely within the same minor version, so that's
    # the requirement we enforce.

    current = System.version()

    same_minor =
      current
      |> Version.parse!()
      |> Map.replace!(:patch, 0)
      |> Version.to_string()

    # Make sure Livebook does not enforce a higher patch version
    min_version =
      if Version.match?(same_minor, @elixir_version_requirement) do
        same_minor
      else
        current
      end

    "~> " <> min_version
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Attached do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(runtime) do
    [
      {"Type", "Attached node"},
      {"Node name", Atom.to_string(runtime.node)}
    ]
  end

  def connect(runtime) do
    Livebook.Runtime.Attached.__connect__(runtime)
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    RuntimeServer.stop(runtime.server_pid)
    Node.disconnect(runtime.node)
    :ok
  end

  def duplicate(runtime) do
    Livebook.Runtime.Attached.new(runtime.node, runtime.cookie)
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

  def relabel_file(runtime, file_id, new_file_id) do
    RuntimeServer.relabel_file(runtime.server_pid, file_id, new_file_id)
  end

  def transfer_file(runtime, path, file_id, callback) do
    RuntimeServer.transfer_file(runtime.server_pid, path, file_id, callback)
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

  def fixed_dependencies?(_runtime), do: true

  def add_dependencies(_runtime, _code, _dependencies) do
    raise "not supported"
  end

  def has_dependencies?(runtime, dependencies) do
    RuntimeServer.has_dependencies?(runtime.server_pid, dependencies)
  end

  def snippet_definitions(_runtime) do
    Livebook.Runtime.Definitions.snippet_definitions()
  end

  def search_packages(_runtime, _send_to, _search) do
    raise "not supported"
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
