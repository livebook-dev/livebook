defmodule Livebook.WebSocket.Server do
  @moduledoc false
  use GenServer

  require Logger

  import Livebook.WebSocket.Client, only: [is_frame: 1]

  alias Livebook.WebSocket.Client

  @timeout 10_000

  defstruct [
    :conn_opts,
    :conn,
    :websocket,
    :caller,
    :status,
    :resp_headers,
    :resp_body,
    :ref,
    connected?: false,
    closing?: false
  ]

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Connects the WebSocket client.
  """
  @spec connect(pid(), String.t(), list({String.t(), String.t()})) ::
          {:ok, :connected} | {:error, Client.ws_error() | Client.mint_error()}
  def connect(pid, url, headers \\ []) do
    GenServer.call(pid, {:connect, url, headers}, @timeout)
  end

  def connected?(pid) do
    GenServer.call(pid, :connected?)
  end

  @doc """
  Disconnects the WebSocket client.
  """
  @spec disconnect(pid()) :: :ok
  def disconnect(pid) do
    GenServer.cast(pid, :disconnect)
  end

  @doc """
  Sends a message to the WebSocket server the message request.
  """
  @spec send_message(pid(), Client.frame()) ::
          :ok | {:error, Client.ws_error() | Client.mint_error()}
  def send_message(pid, frame) when is_frame(frame) do
    GenServer.call(pid, {:send_message, frame}, @timeout)
  end

  ## GenServer callbacks

  @impl true
  def init(_) do
    {:ok, %__MODULE__{}}
  end

  @impl true
  def handle_call({:connect, url, headers}, from, state) do
    state = %{state | caller: from, conn_opts: [url: url, headers: headers]}

    case Client.connect(url, headers) do
      {:ok, conn, ref} ->
        {:noreply, %{state | conn: conn, ref: ref}}

      {:error, _reason} = error ->
        Process.send_after(self(), :reconnect, 2_000)
        {:reply, error, state}

      {:error, conn, reason} ->
        Process.send_after(self(), :reconnect, 2_000)
        {:reply, {:error, reason}, %{state | conn: conn}}
    end
  end

  @dialyzer {:nowarn_function, handle_call: 3}

  def handle_call({:send_message, frame}, _from, state) do
    case Client.send(state.conn, state.websocket, state.ref, frame) do
      {:ok, conn, websocket} ->
        {:reply, :ok, %{state | conn: conn, websocket: websocket}}

      {:error, %Mint.WebSocket{} = websocket, reason} ->
        {:reply, {:error, reason}, %{state | websocket: websocket}}

      {:error, conn, reason} ->
        {:reply, {:error, reason}, %{state | conn: conn}}
    end
  end

  def handle_call(:connected?, _from, state) do
    {:reply, state.connected?, state}
  end

  @impl true
  def handle_cast(:disconnect, state) do
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
  def handle_info(:reconnect, state) do
    state = %{
      state
      | conn: nil,
        ref: nil,
        websocket: nil,
        status: nil,
        resp_headers: nil,
        resp_body: nil,
        connected?: false
    }

    case Client.connect(state.conn_opts[:url], state.conn_opts[:headers]) do
      {:ok, conn, ref} ->
        {:noreply, %{state | conn: conn, ref: ref}}

      {:error, _} ->
        Process.send_after(self(), :reconnect, 2_000)
        {:noreply, state}
    end
  end

  def handle_info(message, state) do
    case Client.receive(state.conn, state.ref, state.websocket, message) do
      {:ok, conn, websocket, :connected} ->
        {:noreply,
         %{state | conn: conn, websocket: websocket}
         |> reply({:ok, :connected})
         |> Map.replace!(:connected?, true)}

      {:ok, conn, websocket, response} ->
        state = %{state | conn: conn, websocket: websocket}
        {:noreply, reply(state, {:ok, response})}

      {:error, conn, websocket, response} ->
        state = %{state | conn: conn, websocket: websocket}
        {:noreply, reply(state, {:error, response})}

      {:error, conn, %Mint.TransportError{reason: :closed} = error} ->
        state = %{state | conn: conn, connected?: false}
        send(self(), :reconnect)
        {:noreply, reply(state, {:error, error})}

      {:error, conn, reason} ->
        state = %{state | conn: conn}
        {:noreply, reply(state, {:error, reason})}

      {:error, _} ->
        {:noreply, state}
    end
  end

  # Private

  defp reply(state, data) do
    response = build_response(data)

    case state do
      %{caller: nil} -> :ok
      %{caller: {pid, _}, connected?: true} -> send(pid, response)
      %{caller: caller} -> GenServer.reply(caller, response)
    end

    state
  end

  defp build_response({:ok, :connected} = response), do: response

  defp build_response({:ok, %Client.Response{body: nil, status: nil}} = response), do: response

  defp build_response({:error, %Client.Response{body: nil, status: status} = response}) do
    {:error, %{response | body: Plug.Conn.Status.reason_phrase(status)}}
  end

  defp build_response({:ok, %Client.Response{body: body} = response}) do
    case LivebookProto.Response.decode(body) do
      %{type: {:error, _} = error} -> {:error, %{response | body: error}}
      %{type: result} -> {:ok, %{response | body: result}}
    end
  end

  defp build_response({:error, %Client.Response{body: _}} = response), do: response
  defp build_response({:error, reason}), do: {:error, %Client.Response{body: reason}}
end
