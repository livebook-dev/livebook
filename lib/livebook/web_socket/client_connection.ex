defmodule Livebook.WebSocket.ClientConnection do
  @moduledoc false
  use Connection

  require Logger

  alias Livebook.WebSocket
  alias Livebook.WebSocket.Client

  @timeout 10_000
  @backoff 1_490

  defstruct [:url, :listener, :headers, :http_conn, :websocket, :ref, id: 0, reply: %{}]

  @doc """
  Starts a new WebSocket connection with given URL and headers.
  """
  @spec start_link(pid(), String.t(), Mint.Types.headers()) :: GenServer.on_start()
  def start_link(listener, url, headers \\ []) do
    Connection.start_link(__MODULE__, {listener, url, headers})
  end

  @doc """
  Sends a Request to given WebSocket Server.
  """
  @spec send_request(pid(), WebSocket.proto()) :: {atom(), term()}
  def send_request(conn, %_struct{} = data) do
    Connection.call(conn, {:request, data}, @timeout)
  end

  ## Connection callbacks

  @impl true
  def init({listener, url, headers}) do
    state = %__MODULE__{listener: listener, url: url, headers: headers}
    {:connect, :init, state}
  end

  @impl true
  def connect(_, state) do
    case Client.connect(state.url, state.headers) do
      {:ok, conn, websocket, ref} ->
        send(state.listener, {:connect, :ok, :connected})
        send(self(), {:loop_ping, ref})

        {:ok, %{state | http_conn: conn, ref: ref, websocket: websocket}}

      {:transport_error, reason} ->
        send(state.listener, {:connect, :error, reason})

        {:backoff, @backoff, state}

      {:server_error, binary} ->
        {:response, %{type: {:error, error}}} = decode_response_or_event(binary)
        send(state.listener, {:connect, :error, error.details})

        {:backoff, @backoff, state}
    end
  end

  @dialyzer {:nowarn_function, disconnect: 2}

  @impl true
  def disconnect(info, state) do
    case info do
      {:close, from} -> Logger.debug("Received close from: #{inspect(from)}")
      {:error, :closed} -> Logger.error("Connection closed")
      {:error, reason} -> Logger.error("Connection error: #{inspect(reason)}")
    end

    {:connect, :reconnect, state}
  end

  ## GenServer callbacks

  @impl true
  def handle_call({:request, data}, caller, state) do
    id = state.id
    frame = LivebookProto.build_request_frame(data, id)
    reply = Map.put(state.reply, id, caller)

    case Client.send(state.http_conn, state.websocket, state.ref, frame) do
      {:ok, conn, websocket} ->
        {:noreply, %{state | http_conn: conn, websocket: websocket, id: id + 1, reply: reply}}

      {:error, conn, websocket, reason} ->
        {:reply, {:error, reason}, %{state | http_conn: conn, websocket: websocket}}
    end
  end

  @loop_ping_delay 5_000

  @impl true
  def handle_info({:loop_ping, ref}, state) when ref == state.ref and is_reference(ref) do
    case Client.send(state.http_conn, state.websocket, state.ref, :ping) do
      {:ok, conn, websocket} ->
        Process.send_after(self(), {:loop_ping, state.ref}, @loop_ping_delay)
        {:noreply, %{state | http_conn: conn, websocket: websocket}}

      {:error, conn, websocket, _reason} ->
        {:noreply, %{state | http_conn: conn, websocket: websocket}}
    end
  end

  def handle_info({:loop_ping, _another_ref}, state), do: {:noreply, state}

  def handle_info({:tcp_closed, _port} = message, state),
    do: handle_websocket_message(message, state)

  def handle_info({:tcp, _port, _data} = message, state),
    do: handle_websocket_message(message, state)

  def handle_info(_message, state), do: {:noreply, state}

  # Private

  def handle_websocket_message(message, state) do
    case Client.receive(state.http_conn, state.ref, state.websocket, message) do
      {:ok, conn, websocket, data} ->
        state = %{state | http_conn: conn, websocket: websocket}
        {:noreply, send_received(data, state)}

      {:server_error, conn, websocket, reason} ->
        send(state.listener, {:connect, :error, reason})

        {:connect, :receive, %{state | http_conn: conn, websocket: websocket}}
    end
  end

  defp send_received([], state), do: state

  defp send_received([_ | _] = binaries, state) do
    for binary <- binaries, reduce: state do
      acc ->
        case decode_response_or_event(binary) do
          {:response, %{id: -1, type: {:error, %{details: reason}}}} ->
            reply_to_all({:error, reason}, acc)

          {:response, %{id: id, type: {:error, %{details: reason}}}} ->
            reply_to_id(id, {:error, reason}, acc)

          {:response, %{id: id, type: {:changeset, %{errors: field_errors}}}} ->
            reply_to_id(id, {:changeset_error, to_changeset_errors(field_errors)}, acc)

          {:response, %{id: id, type: result}} ->
            reply_to_id(id, result, acc)

          {:event, %{type: {name, data}}} ->
            send(acc.listener, {:event, name, data})
            acc
        end
    end
  end

  defp to_changeset_errors(field_errors) do
    for %{field: field, details: errors} <- field_errors, into: %{} do
      {String.to_atom(field), errors}
    end
  end

  defp reply_to_all(message, state) do
    for {_id, caller} <- state.reply do
      Connection.reply(caller, message)
    end

    state
  end

  defp reply_to_id(id, message, state) do
    {caller, reply} = Map.pop(state.reply, id)
    if caller, do: Connection.reply(caller, message)

    %{state | reply: reply}
  end

  defp decode_response_or_event(data) do
    case LivebookProto.Response.decode(data) do
      %{type: nil} -> {:event, LivebookProto.Event.decode(data)}
      response -> {:response, response}
    end
  end
end
