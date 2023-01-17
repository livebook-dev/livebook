defmodule Livebook.Hubs.EnterpriseClient do
  @moduledoc false
  use GenServer

  alias Livebook.Hubs.Broadcasts
  alias Livebook.Hubs.Enterprise
  alias Livebook.Secrets.Secret
  alias Livebook.WebSocket.ClientConnection

  @registry Livebook.HubsRegistry
  @supervisor Livebook.HubsSupervisor

  defstruct [:server, :hub, connected?: false, secrets: []]

  @doc """
  Connects the Enterprise client with WebSocket server.
  """
  @spec start_link(Enterprise.t()) :: GenServer.on_start()
  def start_link(%Enterprise{} = enterprise) do
    GenServer.start_link(__MODULE__, enterprise, name: registry_name(enterprise.id))
  end

  @doc """
  Stops the WebSocket server.
  """
  @spec stop(String.t()) :: :ok
  def stop(id) do
    if pid = GenServer.whereis(registry_name(id)) do
      DynamicSupervisor.terminate_child(@supervisor, pid)
    end

    :ok
  catch
    :exit, _ -> :ok
  end

  @doc """
  Sends a request to the WebSocket server.
  """
  @spec send_request(pid(), WebSocket.proto()) :: {atom(), term()}
  def send_request(pid, %_struct{} = data) do
    ClientConnection.send_request(GenServer.call(pid, :get_server), data)
  end

  @doc """
  Returns a list of cached secrets.
  """
  @spec list_cached_secrets(pid()) :: list(Secret.t())
  def list_cached_secrets(pid) do
    GenServer.call(pid, :list_cached_secrets)
  end

  @doc """
  Returns if the given enterprise is connected.
  """
  @spec connected?(String.t()) :: boolean()
  def connected?(id) do
    GenServer.call(registry_name(id), :connected?)
  catch
    :exit, _ -> false
  end

  ## GenServer callbacks

  @impl true
  def init(%Enterprise{url: url, token: token} = enterprise) do
    headers = [{"X-Auth-Token", token}]
    {:ok, pid} = ClientConnection.start_link(self(), url, headers)

    {:ok, %__MODULE__{hub: enterprise, server: pid}}
  end

  @impl true
  def handle_call(:get_server, _caller, state) do
    {:reply, state.server, state}
  end

  def handle_call(:list_cached_secrets, _caller, state) do
    {:reply, state.secrets, state}
  end

  def handle_call(:connected?, _caller, state) do
    {:reply, state.connected?, state}
  end

  @impl true
  def handle_info({:connect, :ok, _}, state) do
    Broadcasts.hub_connected()
    {:noreply, %{state | connected?: true}}
  end

  def handle_info({:connect, :error, reason}, state) do
    Broadcasts.hub_connection_failed(reason)
    {:noreply, %{state | connected?: false}}
  end

  def handle_info({:disconnect, :error, reason}, state) do
    Broadcasts.hub_disconnection_failed(reason)
    {:noreply, %{state | connected?: false}}
  end

  def handle_info({:event, :secret_created, %{name: name, value: value}}, state) do
    secret = %Secret{name: name, value: value, origin: state.hub.id}
    Broadcasts.secret_created(secret)

    {:noreply, put_secret(state, secret)}
  end

  def handle_info({:event, :secret_updated, %{name: name, value: value}}, state) do
    secret = %Secret{name: name, value: value, origin: state.hub.id}
    Broadcasts.secret_updated(secret)

    {:noreply, put_secret(state, secret)}
  end

  def handle_info({:disconnect, :ok, :disconnected}, state) do
    Broadcasts.hub_disconnected()
    {:stop, :normal, state}
  end

  # Private

  defp registry_name(id) do
    {:via, Registry, {@registry, id}}
  end

  defp put_secret(state, secret) do
    secrets = Enum.reject(state.secrets, &(&1.name == secret.name and &1.origin == secret.origin))
    %{state | secrets: [secret | secrets]}
  end
end
