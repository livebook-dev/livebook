defmodule Livebook.Runtime.RemoteUtils do
  # Shared code for runtimes using a remote node.

  require Logger

  @doc """
  The port that the remote runtime node uses for distribution.
  """
  @spec remote_port() :: pos_integer()
  def remote_port(), do: 44444

  @doc """
  Encodes information for the remote node.

  The returned value should be passed when starting the remote node
  via the LIVEBOOK_RUNTIME environment variable.
  """
  @spec encode_runtime_data(String.t()) :: String.t()
  def encode_runtime_data(node_base) do
    %{
      node_base: node_base,
      cookie: Node.get_cookie(),
      dist_port: remote_port()
    }
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  @doc """
  Discovers a free TCP port.
  """
  @spec get_free_port!() :: pos_integer()
  def get_free_port!() do
    {:ok, socket} = :gen_tcp.listen(0, active: false, reuseaddr: true)
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)
    port
  end

  @doc """
  Fetches information from the remote runtime node.
  """
  @spec fetch_runtime_info(node()) :: %{pid: pid()}
  def fetch_runtime_info(child_node) do
    # Note: it is Livebook that starts the runtime node, so we know
    # that the node runs Livebook release of the exact same version
    #
    # Also, the remote node already has all the runtime modules in
    # the code path, compiled for its Elixir version, so we don't
    # need to check for matching Elixir version.

    :erpc.call(child_node, :persistent_term, :get, [:livebook_runtime_info])
  end

  @doc """
  Attempts connecting to the given node.

  Makes several connect attempts over a few seconds.
  """
  @spec connect(node()) :: :ok | {:error, String.t()}
  def connect(node) do
    connect_loop(node, 40, 250)
  end

  defp connect_loop(_node, 0, _interval) do
    {:error, "could not establish connection with the node"}
  end

  defp connect_loop(node, attempts, interval) do
    if Node.connect(node) do
      :ok
    else
      Process.sleep(interval)
      connect_loop(node, attempts - 1, interval)
    end
  end

  @doc """
  Starts a runtime server on the remote node.
  """
  @spec initialize_node(node()) :: pid()
  def initialize_node(child_node) do
    init_opts = [
      runtime_server_opts: [
        extra_smart_cell_definitions: Livebook.Runtime.Definitions.smart_cell_definitions()
      ]
    ]

    Livebook.Runtime.ErlDist.initialize(child_node, init_opts)
  end

  @doc """
  Wraps a potentially long operation.

  Logs operation duration after completion. On failure, also logs the
  error.
  """
  @spec with_log(String.t(), (-> term())) :: term()
  def with_log(name, fun) do
    {microseconds, result} = :timer.tc(fun)
    milliseconds = div(microseconds, 1000)

    case result do
      {:error, error} ->
        Logger.debug("#{name} FAILED in #{milliseconds}ms, error: #{error}")

      _ ->
        Logger.debug("#{name} finished in #{milliseconds}ms")
    end

    result
  end
end
