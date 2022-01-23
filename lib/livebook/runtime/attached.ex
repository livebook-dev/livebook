defmodule Livebook.Runtime.Attached do
  @moduledoc false

  # A runtime backed by an Elixir node managed externally.
  #
  # Such node must be already started and available,
  # Livebook doesn't manage its lifetime in any way
  # and only loads/unloads the necessary elements.
  # The node can be an ordinary Elixir runtime,
  # a Mix project shell, a running release or anything else.

  defstruct [:node, :cookie, :server_pid]

  @type t :: %__MODULE__{
          node: node(),
          cookie: atom(),
          server_pid: pid()
        }

  @doc """
  Checks if the given node is available for use and initializes
  it with Livebook-specific modules and processes.
  """
  @spec init(node(), atom()) :: {:ok, t()} | {:error, :unreachable}
  def init(node, cookie \\ Node.get_cookie()) do
    # Set cookie for connecting to this specific node
    Node.set_cookie(node, cookie)

    case Node.ping(node) do
      :pong ->
        opts = [parent_node: node()]
        server_pid = Livebook.Runtime.ErlDist.initialize(node, opts)
        {:ok, %__MODULE__{node: node, cookie: cookie, server_pid: server_pid}}

      :pang ->
        {:error, :unreachable}
    end
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Attached do
  alias Livebook.Runtime.ErlDist

  def connect(runtime) do
    ErlDist.RuntimeServer.set_owner(runtime.server_pid, self())
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    ErlDist.RuntimeServer.stop(runtime.server_pid)
  end

  def evaluate_code(runtime, code, locator, prev_locator, opts \\ []) do
    ErlDist.RuntimeServer.evaluate_code(runtime.server_pid, code, locator, prev_locator, opts)
  end

  def forget_evaluation(runtime, locator) do
    ErlDist.RuntimeServer.forget_evaluation(runtime.server_pid, locator)
  end

  def drop_container(runtime, container_ref) do
    ErlDist.RuntimeServer.drop_container(runtime.server_pid, container_ref)
  end

  def handle_intellisense(runtime, send_to, ref, request, locator) do
    ErlDist.RuntimeServer.handle_intellisense(runtime.server_pid, send_to, ref, request, locator)
  end

  def duplicate(runtime) do
    case Livebook.Runtime.Attached.init(runtime.node, runtime.cookie) do
      {:ok, runtime} -> {:ok, runtime}
      {:error, :unreachable} -> {:error, "node #{inspect(runtime.node)} is unreachable"}
    end
  end

  def standalone?(_runtime), do: false

  def read_file(runtime, path) do
    ErlDist.RuntimeServer.read_file(runtime.server_pid, path)
  end
end
