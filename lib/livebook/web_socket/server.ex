defmodule Livebook.WebSocket.Server do
  @moduledoc false
  use GenServer

  require Logger

  import Livebook.WebSocket.Client, only: [is_frame: 1]

  alias Livebook.WebSocket.Client

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
  def send_message(socket, frame) when is_frame(frame) do
    GenServer.cast(socket, {:send_message, frame})
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

  @impl true
  def handle_cast(:close, state) do
    Client.disconnect(state.conn, state.websocket, state.ref)

    {:stop, :normal, state}
  end

  def handle_cast({:send_message, frame}, state) do
    case Client.send(state.conn, state.websocket, state.ref, frame) do
      {:ok, conn, websocket} ->
        {:noreply, %{state | conn: conn, websocket: websocket}}

      {:error, %Mint.WebSocket{} = websocket, _reason} ->
        {:noreply, %{state | websocket: websocket}}

      {:error, conn, _reason} ->
        {:noreply, %{state | conn: conn}}
    end
  end

  @impl true
  def handle_info(message, state) do
    case Client.receive(state.conn, state.ref, state.websocket, message) do
      {:ok, conn, websocket, response} ->
        state = %{state | conn: conn, websocket: websocket}
        {:noreply, reply(state, {:ok, response})}

      {:error, conn, websocket, response} ->
        state = %{state | conn: conn, websocket: websocket}
        {:noreply, reply(state, {:error, response})}

      {:error, conn, response} ->
        state = %{state | conn: conn}
        {:noreply, reply(state, {:error, response})}

      {:error, _} ->
        {:noreply, state}
    end
  end

  # Private

  defp reply(%{caller: nil} = state, response) do
    Logger.warn("The caller is nil, so we can't reply the message: #{inspect(response)}")
    state
  end

  defp reply(state, response) do
    GenServer.reply(state.caller, response)

    state
  end
end
