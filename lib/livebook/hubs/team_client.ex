defmodule Livebook.Hubs.TeamClient do
  @moduledoc false
  use GenServer
  require Logger

  alias Livebook.Hubs.Broadcasts
  alias Livebook.Hubs.Team
  alias Livebook.Teams
  alias Livebook.Teams.Connection

  @registry Livebook.HubsRegistry
  @supervisor Livebook.HubsSupervisor

  defstruct [:hub, :connection_error, :derived_keys, connected?: false, secrets: []]

  @type registry_name :: {:via, Registry, {Livebook.HubsRegistry, String.t()}}

  @doc """
  Connects the Team client with WebSocket server.
  """
  @spec start_link(Team.t()) :: GenServer.on_start()
  def start_link(%Team{} = team) do
    GenServer.start_link(__MODULE__, team, name: registry_name(team.id))
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
  end

  @doc """
  Returns a list of cached secrets.
  """
  @spec get_secrets(String.t()) :: list(Secret.t())
  def get_secrets(id) do
    GenServer.call(registry_name(id), :get_secrets)
  end

  @doc """
  Returns the latest error from connection.
  """
  @spec get_connection_error(String.t()) :: String.t() | nil
  def get_connection_error(id) do
    GenServer.call(registry_name(id), :get_connection_error)
  catch
    :exit, _ -> "connection refused"
  end

  @doc """
  Returns if the Team client is connected.
  """
  @spec connected?(String.t()) :: boolean()
  def connected?(id) do
    GenServer.call(registry_name(id), :connected?)
  catch
    :exit, _ -> false
  end

  ## GenServer callbacks

  @impl true
  def init(%Team{offline: nil} = team) do
    derived_keys = Teams.derive_keys(team.teams_key)

    headers = [
      {"x-lb-version", to_string(Application.spec(:livebook, :vsn))},
      {"x-user", to_string(team.user_id)},
      {"x-org", to_string(team.org_id)},
      {"x-org-key", to_string(team.org_key_id)},
      {"x-session-token", team.session_token}
    ]

    {:ok, _pid} = Connection.start_link(self(), headers)
    {:ok, %__MODULE__{hub: team, derived_keys: derived_keys}}
  end

  def init(%Team{} = team) do
    derived_keys = Teams.derive_keys(team.teams_key)

    {:ok, %__MODULE__{hub: team, secrets: team.offline.secrets, derived_keys: derived_keys}}
  end

  @impl true
  def handle_call(:get_connection_error, _caller, state) do
    {:reply, state.connection_error, state}
  end

  def handle_call(:connected?, _caller, state) do
    {:reply, state.connected?, state}
  end

  def handle_call(:get_secrets, _caller, state) do
    {:reply, state.secrets, state}
  end

  @impl true
  def handle_info(:connected, state) do
    Broadcasts.hub_connected(state.hub.id)
    {:noreply, %{state | connected?: true, connection_error: nil}}
  end

  def handle_info({:connection_error, reason}, state) do
    Broadcasts.hub_connection_failed(state.hub.id, reason)
    {:noreply, %{state | connected?: false, connection_error: reason}}
  end

  def handle_info({:server_error, reason}, state) do
    Broadcasts.hub_server_error(state.hub.id, "#{state.hub.hub_name}: #{reason}")
    :ok = Livebook.Hubs.delete_hub(state.hub.id)

    {:noreply, %{state | connected?: false}}
  end

  def handle_info({:event, topic, data}, state) do
    Logger.debug("Received event #{topic} with data: #{inspect(data)}")

    {:noreply, handle_event(topic, data, state)}
  end

  # Private

  defp registry_name(id) do
    {:via, Registry, {@registry, id}}
  end

  defp put_secret(state, secret) do
    state = remove_secret(state, secret)
    %{state | secrets: [secret | state.secrets]}
  end

  defp remove_secret(state, secret) do
    %{state | secrets: Enum.reject(state.secrets, &(&1.name == secret.name))}
  end

  defp build_secret(state, %{name: name, value: value}) do
    {secret_key, sign_secret} = state.derived_keys
    {:ok, decrypted_value} = Teams.decrypt(value, secret_key, sign_secret)

    %Livebook.Secrets.Secret{
      name: name,
      value: decrypted_value,
      hub_id: state.hub.id
    }
  end

  defp handle_event(:secret_created, secret_created, state) do
    secret = build_secret(state, secret_created)
    Broadcasts.secret_created(secret)

    put_secret(state, secret)
  end

  defp handle_event(:secret_updated, secret_updated, state) do
    secret = build_secret(state, secret_updated)
    Broadcasts.secret_updated(secret)

    put_secret(state, secret)
  end

  defp handle_event(:secret_deleted, secret_deleted, state) do
    if secret = Enum.find(state.secrets, &(&1.name == secret_deleted.name)) do
      Broadcasts.secret_deleted(secret)
      remove_secret(state, secret)
    else
      state
    end
  end

  defp handle_event(:user_connected, %{secrets: secrets}, state) do
    created_secrets =
      Enum.reject(secrets, fn secret ->
        Enum.find(state.secrets, &(&1.name == secret.name and &1.value == secret.value))
      end)

    deleted_secrets =
      Enum.reject(state.secrets, fn secret ->
        Enum.find(secrets, &(&1.name == secret.name))
      end)

    updated_secrets =
      Enum.filter(secrets, fn secret ->
        Enum.find(state.secrets, &(&1.name == secret.name and &1.value != secret.value))
      end)

    secrets_by_topic = [
      secret_deleted: deleted_secrets,
      secret_created: created_secrets,
      secret_updated: updated_secrets
    ]

    for {topic, secrets} <- secrets_by_topic,
        secret <- secrets,
        reduce: state,
        do: (acc -> handle_event(topic, secret, acc))
  end
end
