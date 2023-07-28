defmodule Livebook.Teams.Connection do
  @moduledoc false

  @behaviour :gen_statem

  require Logger

  alias Livebook.WebSocket
  alias Livebook.Teams.WebSocket

  @backoff 5_000
  @no_state :no_state
  @loop_ping_delay 5_000

  @websocket_messages [:ssl, :tcp, :ssl_closed, :tcp_closed, :ssl_error, :tcp_error]

  defstruct [:listener, :headers, :http_conn, :websocket, :ref]

  @doc """
  Starts a new WebSocket connection with given headers.
  """
  @spec start_link(pid(), Mint.Types.headers()) :: :gen_statem.start_ret()
  def start_link(listener, headers \\ []) do
    :gen_statem.start_link(__MODULE__, {listener, headers}, [])
  end

  ## gen_statem callbacks

  @impl true
  def callback_mode(), do: :handle_event_function

  @impl true
  def init({listener, headers}) do
    data = %__MODULE__{listener: listener, headers: headers}
    {:ok, @no_state, data, {:next_event, :internal, :connect}}
  end

  @impl true
  def handle_event(event_type, event_data, state, data)

  def handle_event(:internal, :connect, @no_state, %__MODULE__{} = data) do
    case WebSocket.connect(data.headers) do
      {:ok, conn, websocket, ref} ->
        send(data.listener, :connected)
        send(self(), {:loop_ping, ref})

        {:keep_state, %__MODULE__{data | http_conn: conn, ref: ref, websocket: websocket}}

      {:transport_error, reason} ->
        send(data.listener, {:connection_error, reason})
        {:keep_state_and_data, {{:timeout, :backoff}, @backoff, nil}}

      {:server_error, error} ->
        reason = LivebookProto.Error.decode(error).details
        send(data.listener, {:server_error, reason})

        {:keep_state_and_data, {{:timeout, :reconnect}, @backoff, nil}}
    end
  end

  def handle_event({:timeout, :backoff}, nil, _state, _data) do
    {:keep_state_and_data, {:next_event, :internal, :connect}}
  end

  def handle_event({:timeout, :reconnect}, nil, _state, _data) do
    {:keep_state_and_data, {:next_event, :internal, :connect}}
  end

  def handle_event(:info, {:loop_ping, ref}, @no_state, %__MODULE__{ref: ref} = data) do
    case WebSocket.send(data.http_conn, data.websocket, data.ref, :ping) do
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

  def handle_event(:info, message, @no_state, %__MODULE__{} = data)
      when elem(message, 0) in @websocket_messages do
    handle_websocket_message(message, data)
  end

  def handle_event(:info, _message, @no_state, _data) do
    :keep_state_and_data
  end

  # Private

  defp handle_websocket_message(message, %__MODULE__{} = data) do
    case WebSocket.receive(data.http_conn, data.ref, data.websocket, message) do
      {:ok, conn, websocket, binaries} ->
        data = %__MODULE__{data | http_conn: conn, websocket: websocket}

        for binary <- binaries do
          %{type: {topic, message}} = LivebookProto.Event.decode(binary)
          send(data.listener, {:event, topic, message})
        end

        {:keep_state, data}

      {:server_error, conn, websocket, reason} ->
        send(data.listener, {:connection_error, reason})
        data = %__MODULE__{data | http_conn: conn, websocket: websocket}

        {:keep_state, data, {:next_event, :internal, :connect}}
    end
  end
end
