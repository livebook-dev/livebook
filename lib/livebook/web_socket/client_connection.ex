defmodule Livebook.WebSocket.ClientConnection do
  @moduledoc false

  @behaviour :gen_statem

  require Logger

  alias Livebook.WebSocket
  alias Livebook.WebSocket.Client

  @timeout 10_000
  @backoff 5_000
  @no_state :no_state
  @loop_ping_delay 5_000

  defstruct [:url, :listener, :headers, :http_conn, :websocket, :ref, id: 0, reply: %{}]

  @doc """
  Starts a new WebSocket connection with given URL and headers.
  """
  @spec start_link(pid(), String.t(), Mint.Types.headers()) :: :gen_statem.start_ret()
  def start_link(listener, url, headers \\ []) do
    :gen_statem.start_link(__MODULE__, {listener, url, headers}, [])
  end

  @doc """
  Sends a Request to given WebSocket Server.
  """
  @spec send_request(pid(), WebSocket.proto()) :: {atom(), term()}
  def send_request(conn, %_struct{} = data) do
    :gen_statem.call(conn, {:request, data}, @timeout)
  end

  ## gen_statem callbacks

  @impl true
  def callback_mode, do: :handle_event_function

  @impl true
  def init({listener, url, headers}) do
    data = %__MODULE__{listener: listener, url: url, headers: headers}
    {:ok, @no_state, data, {:next_event, :internal, :connect}}
  end

  @impl true
  def handle_event(event_type, event_data, state, data)

  def handle_event(:internal, :connect, @no_state, %__MODULE__{} = data) do
    case Client.connect(data.url, data.headers) do
      {:ok, conn, websocket, ref} ->
        send(data.listener, {:connect, :ok, :connected})
        send(self(), {:loop_ping, ref})

        {:ok, %__MODULE__{data | http_conn: conn, ref: ref, websocket: websocket}}

      {:transport_error, reason} ->
        send(data.listener, {:connect, :error, reason})
        {:keep_state_and_data, {{:timeout, :backoff}, @backoff, nil}}

      {:server_error, binary} ->
        {:response, %{type: {:error, error}}} = decode_response_or_event(binary)
        send(data.listener, {:connect, :error, error.details})
        {:keep_state_and_data, {{:timeout, :reconnect}, @backoff, nil}}
    end
  end

  def handle_event({:timeout, :reconnect}, nil, _state, _data) do
    {:keep_state_and_data, {:next_event, :internal, :connect}}
  end

  def handle_event({:call, from}, {:request, request}, @no_state, %__MODULE__{id: id} = data) do
    frame = LivebookProto.build_request_frame(request, id)
    reply = Map.put(data.reply, id, from)

    case Client.send(data.http_conn, data.websocket, data.ref, frame) do
      {:ok, conn, websocket} ->
        data = %__MODULE__{data | http_conn: conn, websocket: websocket, id: id + 1, reply: reply}
        {:keep_state, data}

      {:error, conn, websocket, reason} ->
        data = %__MODULE__{data | http_conn: conn, websocket: websocket}
        {:keep_state, data, {:reply, from, {:error, reason}}}
    end
  end

  def handle_event(:info, {:loop_ping, ref}, @no_state, %__MODULE__{} = data)
      when ref == data.ref and is_reference(ref) do
    case Client.send(data.http_conn, data.websocket, data.ref, :ping) do
      {:ok, conn, websocket} ->
        Process.send_after(self(), {:loop_ping, data.ref}, @loop_ping_delay)
        {:keep_state, %__MODULE__{data | http_conn: conn, websocket: websocket}}

      {:error, conn, websocket, _reason} ->
        {:keep_state, %__MODULE__{data | http_conn: conn, websocket: websocket}}
    end
  end

  def handle_event(:info, {:loop_ping, _another_ref}, @no_state, _data) do
    :keep_state_and_data
  end

  def handle_event(:info, {:tcp_closed, _port} = message, @no_state, %__MODULE__{} = data) do
    handle_websocket_message(message, data)
  end

  def handle_event(:info, {:tcp, _port, _data} = message, @no_state, %__MODULE__{} = data) do
    handle_websocket_message(message, data)
  end

  def handle_event(:info, _message, @no_state, _data) do
    :keep_state_and_data
  end

  # Private

  defp handle_websocket_message(message, %__MODULE__{} = data) do
    case Client.receive(data.http_conn, data.ref, data.websocket, message) do
      {:ok, conn, websocket, binaries} ->
        data = %__MODULE__{data | http_conn: conn, websocket: websocket}
        data = send_received(binaries, data)
        {:keep_state, data}

      {:server_error, conn, websocket, reason} ->
        send(data.listener, {:connect, :error, reason})
        data = %__MODULE__{data | http_conn: conn, websocket: websocket}
        {:keep_state, data, {:next_event, :internal, :connect}}
    end
  end

  defp send_received([], data), do: data

  defp send_received([_ | _] = binaries, data) do
    for binary <- binaries, reduce: data do
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

  defp reply_to_all(message, %__MODULE__{} = data) do
    for {_id, caller} <- data.reply do
      :gen_statem.reply(caller, message)
    end

    data
  end

  defp reply_to_id(id, message, %__MODULE__{} = data) do
    {caller, reply} = Map.pop(data.reply, id)
    if caller, do: :gen_statem.reply(caller, message)

    %__MODULE__{data | reply: reply}
  end

  defp decode_response_or_event(data) do
    case LivebookProto.Response.decode(data) do
      %{type: nil} -> {:event, LivebookProto.Event.decode(data)}
      response -> {:response, response}
    end
  end
end
