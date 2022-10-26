defmodule Livebook.WebSocket.Client do
  @moduledoc false
  use GenServer

  require Logger

  defstruct [
    :conn,
    :request_ref,
    :websocket,
    :caller,
    :status,
    :resp_headers,
    :resp_body,
    :ref,
    closing?: false
  ]

  defmodule Error do
    defstruct [:body, :status, :headers]
  end

  @ws_path "/livebook/websocket"

  def start(opts \\ []) do
    GenServer.start(__MODULE__, opts)
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Connects the WebSocket client.
  """
  def connect(pid, url, headers \\ []) do
    GenServer.call(pid, {:connect, url, headers})
  end

  @doc """
  Closes the socket
  """
  def close(pid) do
    GenServer.cast(pid, :close)
  end

  @doc """
  Sends a message to the WebSocket server the message request.
  """
  def send_message(socket, data) when is_struct(data) do
    send_message(socket, Protobuf.encode(data))
  end

  def send_message(socket, data) when is_binary(data) do
    GenServer.call(socket, {:send_message, {:text, data}})
  end

  ## GenServer callbacks

  @impl true
  def init(_) do
    {:ok, %__MODULE__{}}
  end

  @impl true
  def handle_call({:connect, url, headers}, from, state) do
    uri = URI.parse(url)

    http_scheme = parse_http_scheme(uri)
    ws_scheme = parse_ws_scheme(uri)
    path = parse_ws_path(uri)

    with {:ok, conn} <- Mint.HTTP.connect(http_scheme, uri.host, uri.port),
         {:ok, conn, ref} <- Mint.WebSocket.upgrade(ws_scheme, conn, path, headers) do
      state = %{state | conn: conn, request_ref: ref, caller: from}
      {:noreply, state}
    else
      {:error, reason} ->
        {:reply, {:error, reason}, state}

      {:error, conn, reason} ->
        {:reply, {:error, reason}, %{state | conn: conn}}
    end
  end

  def handle_call({:send_message, msg}, _from, state) do
    case stream_frame(state, msg) do
      {:ok, state} -> {:reply, :ok, state}
      {:error, state, reason} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_cast(:close, state) do
    do_close(state)
  end

  @impl true
  def handle_info(message, state) do
    case Mint.WebSocket.stream(state.conn, message) do
      {:ok, conn, responses} ->
        state = handle_responses(%{state | conn: conn}, responses)
        if state.closing?, do: do_close(state), else: {:noreply, state}

      {:error, conn, reason, _responses} ->
        state = reply(%{state | conn: conn}, {:error, reason})
        {:noreply, state}

      :unknown ->
        {:noreply, state}
    end
  end

  # Private

  defp parse_ws_path(%URI{query: nil}), do: @ws_path
  defp parse_ws_path(%URI{query: query}), do: @ws_path <> "?" <> query

  defp parse_http_scheme(uri) when uri.scheme in ["http", "ws"], do: :http
  defp parse_http_scheme(uri) when uri.scheme in ["https", "wss"], do: :https

  defp parse_ws_scheme(uri) when uri.scheme in ["http", "ws"], do: :ws
  defp parse_ws_scheme(uri) when uri.scheme in ["https", "wss"], do: :wss

  defp do_close(state) do
    # Streaming a close frame may fail if the server has already closed
    # for writing.
    _ = stream_frame(state, :close)
    Mint.HTTP.close(state.conn)
    {:stop, :normal, state}
  end

  # Encodes a frame as a binary and sends it along the wire, keeping `conn`
  # and `websocket` up to date in `state`.
  defp stream_frame(state, frame) do
    with {:ok, websocket, data} <- Mint.WebSocket.encode(state.websocket, frame),
         {:ok, conn} <- Mint.WebSocket.stream_request_body(state.conn, state.request_ref, data) do
      {:ok, %{state | conn: conn, websocket: websocket}}
    else
      {:error, %Mint.WebSocket{} = websocket, reason} ->
        {:error, %{state | websocket: websocket}, reason}

      {:error, conn, reason} ->
        {:error, %{state | conn: conn}, reason}
    end
  end

  defp handle_responses(state, responses)

  defp handle_responses(%{request_ref: ref} = state, [{:status, ref, status} | rest]) do
    handle_responses(%{state | status: status}, rest)
  end

  defp handle_responses(%{request_ref: ref} = state, [{:headers, ref, resp_headers} | rest]) do
    handle_responses(%{state | resp_headers: resp_headers}, rest)
  end

  defp handle_responses(%{request_ref: ref, websocket: nil} = state, [
         {:data, ref, resp_body} | rest
       ]) do
    handle_responses(%{state | resp_body: resp_body}, rest)
  end

  defp handle_responses(%{request_ref: ref} = state, [{:done, ref} | rest]) do
    case Mint.WebSocket.new(state.conn, ref, state.status, state.resp_headers) do
      {:ok, conn, websocket} ->
        %{
          state
          | conn: conn,
            websocket: websocket,
            status: nil,
            resp_headers: nil,
            resp_body: nil
        }
        |> reply({:ok, :connected})
        |> handle_responses(rest)

      {:error, conn, %Mint.WebSocket.UpgradeFailureError{status_code: status, headers: headers}} ->
        error = %Error{body: state.resp_body, status: status, headers: headers}
        reply(%{state | conn: conn, resp_body: nil}, {:error, error})
    end
  end

  defp handle_responses(%{request_ref: ref, websocket: websocket} = state, [
         {:data, ref, data} | rest
       ]) do
    case Mint.WebSocket.decode(websocket, data) do
      {:ok, websocket, frames} ->
        %{state | websocket: websocket, resp_body: nil}
        |> handle_frames(frames)
        |> handle_responses(rest)

      {:error, websocket, reason} ->
        reply(%{state | websocket: websocket}, {:error, reason})
    end
  end

  defp handle_responses(state, [_response | rest]) do
    handle_responses(state, rest)
  end

  defp handle_responses(state, []), do: state

  defp handle_frames(state, frames) do
    {frames, state} =
      Enum.flat_map_reduce(frames, state, fn
        # deserialize text and binary frames
        {:text, text}, state ->
          response = Livebook.WebSocket.Response.decode(text)
          {[response.type], state}

        # prepare to close the connection when a close frame is received
        {:close, _code, _data}, state ->
          {[], %{state | closing?: true}}
      end)

    {pid, _} = state.caller

    for frame <- frames do
      send(pid, frame)
    end

    state
  end

  # reply to an open GenServer call request if there is one
  defp reply(%{caller: nil} = state, response) do
    Logger.warn("The caller is nil, so we can't reply the message: #{inspect(response)}")
    state
  end

  defp reply(state, response) do
    GenServer.reply(state.caller, response)

    state
  end
end
