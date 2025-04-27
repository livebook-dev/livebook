defmodule Livebook.Teams.WebSocket do
  alias Mint.WebSocket.UpgradeFailureError

  @ws_path "/user/websocket"

  @type conn :: Mint.HTTP.t()
  @type websocket :: Mint.WebSocket.t()
  @type frame :: Mint.WebSocket.frame() | Mint.WebSocket.shorthand_frame()
  @type ref :: Mint.Types.request_ref()

  defguard is_frame(value) when value in [:close, :ping] or elem(value, 0) == :binary

  @doc """
  Connects to the WebSocket server with given headers.
  """
  @spec connect(list({String.t(), String.t()})) ::
          {:ok, conn(), websocket(), ref()}
          | {:transport_error, String.t()}
          | {:server_error, String.t()}
  def connect(headers \\ []) do
    uri = URI.parse(Livebook.Config.teams_url())
    {http_scheme, ws_scheme} = parse_scheme(uri)
    state = %{status: nil, headers: [], body: []}

    opts = Livebook.Utils.mint_connect_options_for_uri(uri)

    opts = Keyword.merge(opts, protocols: [:http1])

    with {:ok, conn} <- Mint.HTTP.connect(http_scheme, uri.host, uri.port, opts),
         {:ok, conn, ref} <- Mint.WebSocket.upgrade(ws_scheme, conn, @ws_path, headers) do
      receive_upgrade(conn, ref, state)
    else
      {:error, exception} ->
        {:transport_error, Exception.message(exception)}

      {:error, conn, exception} ->
        Mint.HTTP.close(conn)
        {:transport_error, Exception.message(exception)}
    end
  end

  defp parse_scheme(uri) when uri.scheme in ["http", "ws"], do: {:http, :ws}
  defp parse_scheme(uri) when uri.scheme in ["https", "wss"], do: {:https, :wss}

  defp receive_upgrade(conn, ref, state) do
    with {:ok, conn} <- Mint.HTTP.set_mode(conn, :passive),
         {:ok, conn, responses} <- Mint.WebSocket.recv(conn, 0, 5_000) do
      handle_upgrade_responses(responses, conn, ref, state)
    else
      {:error, exception} ->
        Mint.HTTP.close(conn)
        {:transport_error, Exception.message(exception)}

      {:error, _websocket, exception, []} ->
        Mint.HTTP.close(conn)
        {:transport_error, Exception.message(exception)}
    end
  end

  defp handle_upgrade_responses([{:status, ref, status} | responses], conn, ref, state) do
    handle_upgrade_responses(responses, conn, ref, %{state | status: status})
  end

  defp handle_upgrade_responses([{:headers, ref, headers} | responses], conn, ref, state) do
    handle_upgrade_responses(responses, conn, ref, %{state | headers: headers})
  end

  defp handle_upgrade_responses([{:data, ref, body} | responses], conn, ref, state) do
    handle_upgrade_responses(responses, conn, ref, %{state | body: [body | state.body]})
  end

  defp handle_upgrade_responses([{:done, ref} | responses], conn, ref, state) do
    case state do
      %{status: 101} ->
        start_websocket(conn, ref, state)

      %{body: []} ->
        handle_upgrade_responses(responses, conn, ref, state)

      %{status: _} ->
        Mint.HTTP.close(conn)
        {:server_error, state.body |> Enum.reverse() |> IO.iodata_to_binary()}
    end
  end

  defp handle_upgrade_responses([], conn, ref, state) do
    receive_upgrade(conn, ref, state)
  end

  defp start_websocket(conn, ref, state) do
    with {:ok, conn, websocket} <- Mint.WebSocket.new(conn, ref, state.status, state.headers),
         {:ok, conn} <- Mint.HTTP.set_mode(conn, :active) do
      {:ok, conn, websocket, ref}
    else
      {:error, exception} ->
        Mint.HTTP.close(conn)
        {:transport_error, Exception.message(exception)}

      {:error, conn, %UpgradeFailureError{}} ->
        Mint.HTTP.close(conn)
        {:server_error, state.body |> Enum.reverse() |> IO.iodata_to_binary()}

      {:error, conn, exception} ->
        Mint.HTTP.close(conn)
        {:transport_error, Exception.message(exception)}
    end
  end

  @doc """
  Disconnects from the given connection, WebSocket and reference.

  If there's no WebSocket connection yet, it'll only close the HTTP connection.
  """
  @spec disconnect(conn(), websocket() | nil, ref()) ::
          {:ok, conn(), websocket() | nil}
          | {:error, conn(), websocket(), term()}
  def disconnect(conn, nil, _ref) do
    {:ok, conn} = Mint.HTTP.close(conn)
    {:ok, conn, nil}
  end

  def disconnect(conn, websocket, ref) do
    with {:ok, conn, websocket} <- send(conn, websocket, ref, :close) do
      {:ok, conn} = Mint.HTTP.close(conn)
      {:ok, conn, websocket}
    end
  end

  @doc """
  Receive the message from the given HTTP connection.

  If the WebSocket isn't connected yet, it will try to get the connection
  response to start a new WebSocket connection.
  """
  @spec receive(conn(), ref(), websocket(), term()) ::
          {:ok, conn(), websocket(), list(binary())}
          | {:closed, conn(), websocket(), list(binary())}
          | {:error, conn(), websocket(), String.t()}
  def receive(conn, ref, websocket, message \\ receive(do: (message -> message))) do
    with {:ok, conn, [{:data, ^ref, data}]} <- Mint.WebSocket.stream(conn, message),
         {:ok, websocket, frames} <- Mint.WebSocket.decode(websocket, data),
         {:ok, response} <- handle_frames(frames) do
      {:ok, conn, websocket, response}
    else
      {:close, response} ->
        with {:ok, conn, websocket} <- disconnect(conn, websocket, ref) do
          {:closed, conn, websocket, response}
        end

      {:error, conn, exception} when is_exception(exception) ->
        {:error, conn, websocket, Exception.message(exception)}

      {:error, conn, exception, []} when is_exception(exception) ->
        {:error, conn, websocket, Exception.message(exception)}

      :unknown ->
        # Message does not belong to this socket. For example, this
        # can be a leftover :tcp_close or :ssl_close from a previously
        # gracefully closed socket.
        {:ok, conn, websocket, []}
    end
  end

  defp handle_frames(frames), do: handle_frames([], frames)

  defp handle_frames(binaries, [{:binary, binary} | rest]),
    do: handle_frames([binary | binaries], rest)

  defp handle_frames(binaries, [{:close, _, _} | _]),
    do: {:close, binaries}

  defp handle_frames(binaries, [_ | rest]), do: handle_frames(binaries, rest)
  defp handle_frames(binaries, []), do: {:ok, binaries}

  @doc """
  Sends a message to the given HTTP Connection and WebSocket connection.
  """
  @spec send(conn(), websocket(), ref(), frame()) ::
          {:ok, conn(), websocket()}
          | {:error, conn(), websocket(), term()}
  def send(conn, websocket, ref, frame) when is_frame(frame) do
    with {:ok, websocket, data} <- Mint.WebSocket.encode(websocket, frame),
         {:ok, conn} <- Mint.WebSocket.stream_request_body(conn, ref, data) do
      {:ok, conn, websocket}
    else
      {:error, %Mint.HTTP1{} = conn, exception} when is_exception(exception) ->
        {:error, conn, websocket, Exception.message(exception)}

      {:error, websocket, exception} when is_exception(exception) ->
        {:error, conn, websocket, Exception.message(exception)}
    end
  end
end
