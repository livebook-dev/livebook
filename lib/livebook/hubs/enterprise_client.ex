defmodule Livebook.Hubs.EnterpriseClient do
  @moduledoc false
  use GenServer

  alias Livebook.Hubs.Enterprise
  alias Livebook.WebSocket.Server

  @pubsub_topic "enterprise"

  defstruct [:server, :hub]

  @doc """
  Connects the Enterprise client with WebSocket server.
  """
  @spec start_link(Enterprise.t()) :: GenServer.on_start()
  def start_link(%Enterprise{} = enterprise) do
    GenServer.start_link(__MODULE__, enterprise)
  end

  @doc """
  Gets the WebSocket server PID.
  """
  @spec get_server(pid()) :: pid()
  def get_server(pid) do
    GenServer.call(pid, :get_server)
  end

  @doc """
  Disconnects the Enterprise client with WebSocket server.
  """
  @spec disconnect(pid()) :: :ok
  def disconnect(pid) do
    GenServer.cast(pid, :disconnect)
  end

  @doc """
  Subscribe to WebSocket Server events.

  ## Messages

    * `{:unknown, :error, reason}`
    * `{:connect, :ok, :waiting_upgrade | :connected}`
    * `{:connect, :error, reason}`
    * `{:disconnect, :ok, :disconnected}`
    * `{:disconnect, :error, reason}`

  """
  @spec subscribe() :: :ok | {:error, {:already_registered, pid()}}
  def subscribe do
    Phoenix.PubSub.subscribe(Livebook.PubSub, @pubsub_topic)
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe() :: :ok
  def unsubscribe do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, @pubsub_topic)
  end

  @doc """

  """
  @spec broadcast_message(any()) :: :ok

  ## GenServer callbacks

  @impl true
  def init(%Enterprise{url: url, token: token} = enterprise) do
    headers = [{"X-Auth-Token", token}]
    {:ok, pid} = Server.start_link(self(), url, headers)

    {:ok, %__MODULE__{hub: enterprise, server: pid}}
  end

  @impl true
  def handle_call(:get_server, _caller, state) do
    {:reply, state.server, state}
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
    {:stop, :normal, state}
  end

  # Private

  # Notifies interested processes about WebSocket Server messages.
  # Broadcasts the given message under the `"enterprise"` topic.
  defp broadcast_message(message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, @pubsub_topic, message)
  end
end
