defmodule Livebook.Teams.Connection do
  @behaviour :gen_statem

  require Logger

  alias Livebook.Teams.WebSocket

  @backoff 5_000
  @no_state :no_state
  @loop_ping_delay 5_000

  @expected_messages [:ssl, :tcp, :ssl_closed, :tcp_closed, :ssl_error, :tcp_error]

  defstruct [:listener, :headers, :http_conn, :websocket, :ref]

  @doc """
  Starts a new WebSocket connection with given headers.
  """
  @spec start_link(pid(), Mint.Types.headers()) :: :gen_statem.start_ret()
  def start_link(listener, headers \\ []) do
    :gen_statem.start_link(__MODULE__, {listener, headers}, [])
  end

  def send_message(conn, message) do
    :gen_statem.call(conn, {:message, message})
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

  def handle_event(:internal, :connect, @no_state, data) do
    case WebSocket.connect(data.headers) do
      {:ok, conn, websocket, ref} ->
        send(data.listener, :connected)
        send(self(), {:loop_ping, ref})

        {:keep_state, %{data | http_conn: conn, ref: ref, websocket: websocket}}

      {:transport_error, reason} ->
        send(data.listener, {:connection_error, reason})
        {:keep_state_and_data, {{:timeout, :backoff}, @backoff, nil}}

      {:server_error, error} ->
        reason = LivebookProto.Error.decode(error).details
        send(data.listener, {:server_error, reason})

        {:keep_state, data}
    end
  end

  def handle_event({:timeout, :backoff}, nil, _state, _data) do
    {:keep_state_and_data, {:next_event, :internal, :connect}}
  end

  def handle_event(:info, {:loop_ping, ref}, @no_state, %{ref: ref} = data) do
    case WebSocket.send(data.http_conn, data.websocket, data.ref, :ping) do
      {:ok, conn, websocket} ->
        Process.send_after(self(), {:loop_ping, data.ref}, @loop_ping_delay)
        {:keep_state, %{data | http_conn: conn, websocket: websocket}}

      {:error, conn, websocket, _reason} ->
        {:keep_state, %{data | http_conn: conn, websocket: websocket}}
    end
  end

  def handle_event(:info, {:loop_ping, _another_ref}, @no_state, _data) do
    :keep_state_and_data
  end

  def handle_event(:info, message, @no_state, data) when elem(message, 0) in @expected_messages do
    handle_websocket_message(message, data)
  end

  def handle_event(:info, message, @no_state, %{http_conn: nil})
      when elem(message, 0) in @expected_messages do
    :keep_state_and_data
  end

  def handle_event(:info, _message, @no_state, _data) do
    :keep_state_and_data
  end

  def handle_event({:call, from}, {:message, message}, @no_state, data) do
    case WebSocket.send(data.http_conn, data.websocket, data.ref, {:binary, message}) do
      {:ok, conn, websocket} ->
        :gen_statem.reply(from, :ok)
        {:keep_state, %{data | http_conn: conn, websocket: websocket}}

      {:error, conn, websocket, reason} ->
        data = %{data | http_conn: conn, websocket: websocket}
        send(data.listener, {:connection_error, reason})
        :gen_statem.reply(from, {:error, reason})

        {:keep_state, data, {:next_event, :internal, :connect}}
    end
  end

  # Private

  defp handle_websocket_message(message, data) do
    case WebSocket.receive(data.http_conn, data.ref, data.websocket, message) do
      {:ok, conn, websocket, binaries} ->
        data = %{data | http_conn: conn, websocket: websocket}

        for binary <- binaries do
          %{type: {topic, message}} = LivebookProto.Event.decode(binary)
          send(data.listener, {:event, topic, message})
        end

        {:keep_state, data}

      {:error, conn, websocket, reason} ->
        send(data.listener, {:connection_error, reason})
        data = %{data | http_conn: conn, websocket: websocket}

        {:keep_state, data, {:next_event, :internal, :connect}}
    end
  end
end
