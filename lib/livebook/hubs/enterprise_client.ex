defmodule Livebook.Hubs.EnterpriseClient do
  @moduledoc false
  use GenServer

  alias Livebook.Hubs.Enterprise
  alias Livebook.Secrets.Secret
  alias Livebook.WebSocket.Server

  @pubsub_topic "enterprise"
  @registry Livebook.HubsRegistry

  defstruct [:server, :hub, secrets: []]

  @doc """
  Connects the Enterprise client with WebSocket server.
  """
  @spec start_link(Enterprise.t()) :: GenServer.on_start()
  def start_link(%Enterprise{} = enterprise) do
    GenServer.start_link(__MODULE__, enterprise, name: registry_name(enterprise))
  end

  @doc """
  Sends a request to the WebSocket server.
  """
  @spec send_request(pid(), WebSocket.proto()) :: {atom(), term()}
  def send_request(pid, %_struct{} = data) do
    Server.send_request(GenServer.call(pid, :get_server), data)
  end

  @doc """
  Returns a list of cached secrets.
  """
  @spec list_cached_secrets(pid()) :: list(Secret.t())
  def list_cached_secrets(pid) do
    GenServer.call(pid, :list_cached_secrets)
  end

  @doc """
  Subscribe to WebSocket Server events.

  ## Messages

    * `{:connect, :ok, :connected}`
    * `{:connect, :error, reason}`
    * `{:secret_created, %Secret{}}`
    * `{:secret_updated, %Secret{}}`

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

  def handle_call(:list_cached_secrets, _caller, state) do
    {:reply, state.secrets, state}
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

  def handle_info({:event, :secret_created, %{name: name, value: value}}, state) do
    secret = %Secret{name: name, value: value}
    broadcast_message({:secret_created, secret})

    {:noreply, put_secret(state, secret)}
  end

  def handle_info({:event, :secret_updated, %{name: name, value: value}}, state) do
    secret = %Secret{name: name, value: value}
    broadcast_message({:secret_updated, secret})

    {:noreply, put_secret(state, secret)}
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

  defp registry_name(%Enterprise{id: id}) do
    {:via, Registry, {@registry, id}}
  end

  defp put_secret(state, %Secret{name: name} = secret) do
    %{state | secrets: [secret | Enum.reject(state.secrets, &(&1.name == name))]}
  end
end
