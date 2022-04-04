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

    # Set cookie for connecting to this specific node
    Node.set_cookie(node, cookie)

    case Node.ping(node) do
      :pong ->
        server_pid =
          Livebook.Runtime.ErlDist.initialize(node,
            node_manager_opts: [parent_node: node()]
          )

        {:ok, %{runtime | server_pid: server_pid}}

      :pang ->
        {:error, "node #{inspect(node)} is unreachable"}
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

  def evaluate_code(runtime, code, locator, base_locator, opts \\ []) do
    RuntimeServer.evaluate_code(runtime.server_pid, code, locator, base_locator, opts)
  end

  def forget_evaluation(runtime, locator) do
    RuntimeServer.forget_evaluation(runtime.server_pid, locator)
  end

  def drop_container(runtime, container_ref) do
    RuntimeServer.drop_container(runtime.server_pid, container_ref)
  end

  def handle_intellisense(runtime, send_to, request, base_locator) do
    RuntimeServer.handle_intellisense(runtime.server_pid, send_to, request, base_locator)
  end

  def read_file(runtime, path) do
    RuntimeServer.read_file(runtime.server_pid, path)
  end

  def start_smart_cell(runtime, kind, ref, attrs, base_locator) do
    RuntimeServer.start_smart_cell(runtime.server_pid, kind, ref, attrs, base_locator)
  end

  def set_smart_cell_base_locator(runtime, ref, base_locator) do
    RuntimeServer.set_smart_cell_base_locator(runtime.server_pid, ref, base_locator)
  end

  def stop_smart_cell(runtime, ref) do
    RuntimeServer.stop_smart_cell(runtime.server_pid, ref)
  end

  def fixed_dependencies?(_runtime), do: true

  def add_dependencies(_runtime, _code, _dependencies) do
    raise "not supported"
  end

  def search_dependencies(_runtime, _send_to, _search) do
    raise "not supported"
  end
end
