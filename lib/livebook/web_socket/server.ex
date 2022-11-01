defmodule Livebook.WebSocket.Server do
  @moduledoc false
  use GenServer

  require Logger

  alias Livebook.WebSocket.Client
  alias Livebook.WebSocket.Client.Response

  defstruct [
    :conn,
    :websocket,
    :caller,
    :status,
    :resp_headers,
    :resp_body,
    :ref,
    closing?: false
  ]

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
  Disconnects the WebSocket client.
  """
  def disconnect(pid) do
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
    case Client.connect(url, headers) do
      {:ok, conn, ref} -> {:noreply, %{state | conn: conn, ref: ref, caller: from}}
      {:error, _reason} = error -> {:reply, error, state}
      {:error, conn, reason} -> {:reply, {:error, reason}, %{state | conn: conn}}
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
         {:ok, conn} <- Mint.WebSocket.stream_request_body(state.conn, state.ref, data) do
      {:ok, %{state | conn: conn, websocket: websocket}}
    else
      {:error, %Mint.WebSocket{} = websocket, reason} ->
        {:error, %{state | websocket: websocket}, reason}

      {:error, conn, reason} ->
        {:error, %{state | conn: conn}, reason}
    end
  end

  defp handle_responses(state, responses)

  defp handle_responses(%{ref: ref} = state, [{:status, ref, status} | rest]) do
    handle_responses(%{state | status: status}, rest)
  end

  defp handle_responses(%{ref: ref} = state, [{:headers, ref, resp_headers} | rest]) do
    handle_responses(%{state | resp_headers: resp_headers}, rest)
  end

  defp handle_responses(%{ref: ref, websocket: nil} = state, [
         {:data, ref, resp_body} | rest
       ]) do
    handle_responses(%{state | resp_body: decode_binary(resp_body)}, rest)
  end

  defp handle_responses(%{ref: ref} = state, [{:done, ref} | rest]) do
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
        response = %Response{body: state.resp_body, status: status, headers: headers}
        reply(%{state | conn: conn, resp_body: nil}, {:error, response})
    end
  end

  defp handle_responses(%{ref: ref, websocket: websocket} = state, [
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
        {:binary, binary}, state ->
          response = decode_binary(binary)
          {[response.type], state}

        # prepare to close the connection when a close frame is received
        {:close, _code, _data}, state ->
          {[], %{state | closing?: true}}
      end)

    {pid, _} = state.caller

    for frame <- frames, do: send(pid, frame)

    state
  end

  defp decode_binary(binary) when is_binary(binary) do
    LivebookProto.Response.decode(binary)
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
