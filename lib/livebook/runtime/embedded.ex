defmodule Livebook.Runtime.Embedded do
  # A runtime backed by the same node Livebook is running in.
  #
  # This runtime is reserved for specific use cases, where there is
  # no option of starting a separate Elixir OS process.
  #
  # As we run in the Livebook node, all the necessary modules are in
  # place, so we just ensure the node manager process is running and
  # we start a new runtime server. We also disable modules cleanup
  # on termination, since we don't want to unload any modules from
  # the current node.

  defstruct [:server_pid]

  @type t :: %__MODULE__{server_pid: pid() | nil}

  alias Livebook.Runtime.ErlDist

  @doc """
  Returns a new runtime instance.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{}
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
    server_pid =
      ErlDist.initialize(node(),
        node_manager_opts: [unload_modules_on_termination: false]
      )

    runtime = %{runtime | server_pid: server_pid}
    send(caller, {:runtime_connect_done, self(), {:ok, runtime}})
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Embedded do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(_runtime) do
    [{"Type", "Embedded"}]
  end

  def connect(runtime) do
    Livebook.Runtime.Embedded.__connect__(runtime)
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    :ok = RuntimeServer.stop(runtime.server_pid)
  end

  def duplicate(_runtime) do
    Livebook.Runtime.Embedded.new()
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

  def fixed_dependencies?(_runtime) do
    not Keyword.has_key?(config(), :load_packages)
  end

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
    {mod, fun, args} = config()[:load_packages]
    packages = apply(mod, fun, args)
    Livebook.Runtime.Dependencies.search_packages_in_list(packages, send_to, search)
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

  defp config() do
    Application.get_env(:livebook, Livebook.Runtime.Embedded, [])
  end
end
