defmodule Livebook.Hubs.EnterpriseClient do
  @moduledoc false
  use GenServer

  alias Livebook.Hubs.Enterprise
  alias Livebook.WebSocket
  alias Livebook.WebSocket.Server

  defstruct [:server, :hub, reply: %{}]

  @doc """
  Connects the Enterprise client with WebSocket server.
  """
  @spec start_link(Enterprise.t()) :: GenServer.on_start()
  def start_link(%Enterprise{} = enterprise) do
    GenServer.start_link(__MODULE__, enterprise)
  end

  @doc """
  Sends a Request.
  """
  @spec send_request(pid(), WebSocket.proto()) :: :ok
  def send_request(pid, %_struct{} = data) do
    GenServer.call(pid, {:request, data})
  end

  @doc """
  Disconnects the Enterprise client with WebSocket server.
  """
  @spec disconnect(pid()) :: :ok
  def disconnect(pid) do
    GenServer.cast(pid, :disconnect)
  end

  @doc """
  Subscribe to Enterprise Client events.

  ## Messages

    * `{:connect, :error, reason}`

  """
  @spec subscribe() :: :ok | {:error, {:already_registered, pid()}}
  def subscribe do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "enterprise")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe() :: :ok
  def unsubscribe do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "enterprise")
  end

  @doc """
  Notifies interested processes about Enterprise Client messages.

  Broadcasts the given message under the `"enterprise"` topic.
  """
  @spec broadcast_message(any()) :: :ok
  def broadcast_message(message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "enterprise", message)
  end

  ## GenServer callbacks

  @impl true
  def init(%Enterprise{url: url, token: token} = enterprise) do
    headers = [{"X-Auth-Token", token}]

    {:ok, pid} = Server.start_link(url, headers)
    WebSocket.subscribe()

    {:ok, %__MODULE__{hub: enterprise, server: pid}}
  end

  @impl true
  def handle_call({:request, data}, caller, state) do
    {:ok, id} = Server.send_request(state.server, data)

    {:noreply, %{state | reply: Map.put(state.reply, id, caller)}}
  end

  @impl true
  def handle_cast(:disconnect, state) do
    with :ok <- Server.close(state.server) do
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:connect, _, _} = message, state) do
    broadcast_message(message)
    {:noreply, state}
  end

  def handle_info({:disconnect, :error, _} = message, state) do
    broadcast_message(message)
    {:noreply, state}
  end

  def handle_info({:disconnect, :ok, :disconnected}, state) do
    WebSocket.unsubscribe()
    {:stop, :normal, state}
  end

  def handle_info({:response, :error, _reason}, state) do
    {:noreply, state}
  end

  def handle_info({:response, id, result}, state) do
    {caller, reply} = Map.pop(state.reply, id)
    if caller, do: GenServer.reply(caller, result)

    {:noreply, %{state | reply: reply}}
  end
end
