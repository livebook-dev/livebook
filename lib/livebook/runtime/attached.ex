defmodule Livebook.Runtime.Attached do
  @moduledoc false

  # A runtime backed by an Elixir node managed externally.
  #
  # Such node must be already started and available, Livebook doesn't
  # manage its lifetime in any way and only loads/unloads the
  # necessary elements. The node can be an ordinary Elixir runtime,
  # a Mix project shell, a running release or anything else.

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

  @doc """
  Checks if the given node is available for use and initializes
  it with Livebook-specific modules and processes.
  """
  @spec connect(t()) :: {:ok, t()} | {:error, String.t()}
  def connect(runtime) do
    %{node: node, cookie: cookie} = runtime

    # We need to append the hostname on connect because
    # net_kernel has not yet started during new/2.
    node = append_hostname(node)

    # Set cookie for connecting to this specific node
    Node.set_cookie(node, cookie)

    with :ok <- connect_to_node(node),
         :ok <- check_attached_node_version(node) do
      server_pid =
        Livebook.Runtime.ErlDist.initialize(node,
          node_manager_opts: [parent_node: node(), capture_orphan_logs: false]
        )

      {:ok, %{runtime | node: node, server_pid: server_pid}}
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

  @elixir_version_requirement Keyword.fetch!(Mix.Project.config(), :elixir)

  defp check_attached_node_version(node) do
    attached_node_version = :erpc.call(node, System, :version, [])

    if Version.match?(attached_node_version, @elixir_version_requirement) do
      :ok
    else
      {:error,
       "the node uses Elixir #{attached_node_version}, but #{@elixir_version_requirement} is required"}
    end
  end

  defp append_hostname(node) do
    with :nomatch <- :string.find(Atom.to_string(node), "@"),
         <<suffix::binary>> <- :string.find(Atom.to_string(:net_kernel.nodename()), "@") do
      :"#{node}#{suffix}"
    else
      _ -> node
    end
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Attached do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(runtime) do
    [
      {"Type", "Attached"},
      {"Node name", Atom.to_string(runtime.node)}
    ]
  end

  def connect(runtime) do
    Livebook.Runtime.Attached.connect(runtime)
  end

  def connected?(runtime) do
    runtime.server_pid != nil
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    RuntimeServer.stop(runtime.server_pid)
    {:ok, %{runtime | server_pid: nil}}
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

  def handle_intellisense(runtime, send_to, request, parent_locators) do
    RuntimeServer.handle_intellisense(runtime.server_pid, send_to, request, parent_locators)
  end

  def read_file(runtime, path) do
    RuntimeServer.read_file(runtime.server_pid, path)
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

  def disable_dependencies_cache(runtime) do
    RuntimeServer.disable_dependencies_cache(runtime.server_pid)
  end

  def put_system_envs(runtime, envs) do
    RuntimeServer.put_system_envs(runtime.server_pid, envs)
  end

  def delete_system_envs(runtime, names) do
    RuntimeServer.delete_system_envs(runtime.server_pid, names)
  end
end
