defmodule Livebook.Hubs.TeamClient do
  use GenServer
  require Logger

  alias Livebook.FileSystem
  alias Livebook.FileSystems
  alias Livebook.Hubs
  alias Livebook.Secrets
  alias Livebook.Teams

  @registry Livebook.HubsRegistry
  @supervisor Livebook.HubsSupervisor

  defstruct [
    :hub,
    :connection_status,
    :derived_key,
    :deployment_group_id,
    connected?: false,
    secrets: [],
    file_systems: [],
    deployment_groups: []
  ]

  @type registry_name :: {:via, Registry, {Livebook.HubsRegistry, String.t()}}

  @doc """
  Connects the Team client with WebSocket server.
  """
  @spec start_link(Hubs.Team.t()) :: GenServer.on_start()
  def start_link(%Hubs.Team{} = team) do
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
  @spec get_secrets(String.t()) :: list(Secrets.Secret.t())
  def get_secrets(id) do
    GenServer.call(registry_name(id), :get_secrets)
  end

  @doc """
  Returns a list of cached file systems.
  """
  @spec get_file_systems(String.t()) :: list(FileSystem.t())
  def get_file_systems(id) do
    GenServer.call(registry_name(id), :get_file_systems)
  end

  @doc """
  Returns the latest status from connection.
  """
  @spec get_connection_status(String.t()) :: String.t() | nil
  def get_connection_status(id) do
    GenServer.call(registry_name(id), :get_connection_status)
  catch
    :exit, _ -> "connection refused"
  end

  @doc """
  Returns a list of cached deployment groups.
  """
  @spec get_deployment_groups(String.t()) :: list(Teams.DeploymentGroup.t())
  def get_deployment_groups(id) do
    GenServer.call(registry_name(id), :get_deployment_groups)
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

  @doc """
  Enqueues the synchronous event to be handled by the Team client.
  """
  @spec handle_event(String.t(), {atom(), LivebookProto.event_proto()}) :: :ok
  def handle_event(id, {topic, data}) do
    GenServer.cast(registry_name(id), {:event, topic, data})
  end

  ## GenServer callbacks

  @impl true
  def init(%Hubs.Team{offline: nil} = team) do
    derived_key = Teams.derive_key(team.teams_key)

    headers =
      if team.user_id do
        [
          {"x-lb-version", Livebook.Config.app_version()},
          {"x-user", to_string(team.user_id)},
          {"x-org", to_string(team.org_id)},
          {"x-org-key", to_string(team.org_key_id)},
          {"x-session-token", team.session_token}
        ]
      else
        [
          {"x-lb-version", Livebook.Config.app_version()},
          {"x-org", to_string(team.org_id)},
          {"x-org-key", to_string(team.org_key_id)},
          {"x-agent-name", Livebook.Config.agent_name()},
          {"x-agent-key", team.session_token}
        ]
      end

    {:ok, _pid} = Teams.Connection.start_link(self(), headers)
    {:ok, %__MODULE__{hub: team, derived_key: derived_key}}
  end

  def init(%Hubs.Team{} = team) do
    derived_key = Teams.derive_key(team.teams_key)

    {:ok,
     %__MODULE__{
       hub: team,
       secrets: team.offline.secrets,
       file_systems: team.offline.file_systems,
       derived_key: derived_key
     }}
  end

  @impl true
  def handle_call(:get_connection_status, _caller, state) do
    {:reply, state.connection_status, state}
  end

  def handle_call(:connected?, _caller, state) do
    {:reply, state.connected?, state}
  end

  def handle_call(:get_secrets, _caller, %{deployment_group_id: nil} = state) do
    {:reply, state.secrets, state}
  end

  def handle_call(:get_secrets, _caller, state) do
    case find_deployment_group(state.deployment_group_id, state.deployment_groups) do
      nil ->
        {:reply, state.secrets, state}

      %{secrets: agent_secrets} ->
        {:reply, Enum.uniq_by(agent_secrets ++ state.secrets, & &1.name), state}
    end
  end

  def handle_call(:get_file_systems, _caller, state) do
    {:reply, state.file_systems, state}
  end

  def handle_call(:get_deployment_groups, _caller, state) do
    {:reply, state.deployment_groups, state}
  end

  @impl true
  def handle_info(:connected, state) do
    Hubs.Broadcasts.hub_connected(state.hub.id)

    {:noreply, %{state | connected?: true, connection_status: nil}}
  end

  def handle_info({:connection_error, reason}, state) do
    Hubs.Broadcasts.hub_connection_failed(state.hub.id, reason)

    {:noreply, %{state | connected?: false, connection_status: reason}}
  end

  def handle_info({:server_error, reason}, state) do
    Hubs.Broadcasts.hub_server_error(state.hub.id, "#{state.hub.hub_name}: #{reason}")
    :ok = Hubs.delete_hub(state.hub.id)

    {:noreply, %{state | connected?: false}}
  end

  def handle_info({:event, topic, data}, state) do
    Logger.debug("Received event #{topic} with data: #{inspect(data)}")

    {:noreply, handle_event(topic, data, state)}
  end

  @impl true
  def handle_cast({:event, topic, data}, state) do
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

  defp build_secret(state, %{name: name, value: value} = attrs) do
    {:ok, decrypted_value} = Teams.decrypt(value, state.derived_key)

    %Secrets.Secret{
      name: name,
      value: decrypted_value,
      hub_id: state.hub.id,
      deployment_group_id: Map.get(attrs, :deployment_group_id)
    }
  end

  defp put_file_system(state, file_system) do
    state = remove_file_system(state, file_system)

    %{state | file_systems: [file_system | state.file_systems]}
  end

  defp remove_file_system(state, file_system) do
    %{
      state
      | file_systems:
          Enum.reject(state.file_systems, &(&1.external_id == file_system.external_id))
    }
  end

  defp build_file_system(state, file_system) do
    {:ok, decrypted_value} = Teams.decrypt(file_system.value, state.derived_key)

    dumped_data =
      decrypted_value
      |> Jason.decode!()
      |> Map.put("external_id", file_system.id)

    FileSystems.load(file_system.type, dumped_data)
  end

  defp put_deployment_group(state, deployment_group) do
    state = remove_deployment_group(state, deployment_group)
    %{state | deployment_groups: [deployment_group | state.deployment_groups]}
  end

  defp remove_deployment_group(state, deployment_group) do
    %{
      state
      | deployment_groups: Enum.reject(state.deployment_groups, &(&1.id == deployment_group.id))
    }
  end

  defp build_agent_key(agent_key) do
    %Teams.AgentKey{
      id: to_string(agent_key.id),
      key: agent_key.key,
      deployment_group_id: to_string(agent_key.deployment_group_id)
    }
  end

  defp build_deployment_group(state, deployment_group) do
    secrets = Enum.map(deployment_group.secrets, &build_secret(state, &1))
    agent_keys = for agent_key <- deployment_group.agent_keys, do: build_agent_key(agent_key)

    %Teams.DeploymentGroup{
      id: deployment_group.id,
      name: deployment_group.name,
      mode: deployment_group.mode,
      hub_id: state.hub.id,
      secrets: secrets,
      agent_keys: agent_keys,
      clustering: deployment_group.clustering,
      zta_provider: String.to_atom(deployment_group.zta_provider),
      zta_key: deployment_group.zta_key
    }
  end

  defp handle_event(:secret_created, %Secrets.Secret{} = secret, state) do
    Hubs.Broadcasts.secret_created(secret)

    put_secret(state, secret)
  end

  defp handle_event(:secret_created, secret_created, state) do
    handle_event(:secret_created, build_secret(state, secret_created), state)
  end

  defp handle_event(:secret_updated, %Secrets.Secret{} = secret, state) do
    Hubs.Broadcasts.secret_updated(secret)

    put_secret(state, secret)
  end

  defp handle_event(:secret_updated, secret_updated, state) do
    handle_event(:secret_updated, build_secret(state, secret_updated), state)
  end

  defp handle_event(:secret_deleted, secret_deleted, state) do
    if secret = Enum.find(state.secrets, &(&1.name == secret_deleted.name)) do
      Hubs.Broadcasts.secret_deleted(secret)
      remove_secret(state, secret)
    else
      state
    end
  end

  defp handle_event(:file_system_created, %{external_id: _} = file_system, state) do
    Hubs.Broadcasts.file_system_created(file_system)

    put_file_system(state, file_system)
  end

  defp handle_event(:file_system_created, file_system_created, state) do
    handle_event(:file_system_created, build_file_system(state, file_system_created), state)
  end

  defp handle_event(:file_system_updated, %{external_id: _} = file_system, state) do
    Hubs.Broadcasts.file_system_updated(file_system)

    put_file_system(state, file_system)
  end

  defp handle_event(:file_system_updated, file_system_updated, state) do
    handle_event(:file_system_updated, build_file_system(state, file_system_updated), state)
  end

  defp handle_event(:file_system_deleted, %{external_id: _} = file_system, state) do
    Hubs.Broadcasts.file_system_deleted(file_system)

    remove_file_system(state, file_system)
  end

  defp handle_event(:file_system_deleted, %{id: id}, state) do
    if file_system = Enum.find(state.file_systems, &(&1.external_id == id)) do
      handle_event(:file_system_deleted, file_system, state)
    else
      state
    end
  end

  defp handle_event(:deployment_group_created, %Teams.DeploymentGroup{} = deployment_group, state) do
    Teams.Broadcasts.deployment_group_created(deployment_group)

    put_deployment_group(state, deployment_group)
  end

  defp handle_event(:deployment_group_created, deployment_group_created, state) do
    handle_event(
      :deployment_group_created,
      build_deployment_group(state, deployment_group_created),
      state
    )
  end

  defp handle_event(:deployment_group_updated, %Teams.DeploymentGroup{} = deployment_group, state) do
    Teams.Broadcasts.deployment_group_updated(deployment_group)

    put_deployment_group(state, deployment_group)
  end

  defp handle_event(:deployment_group_updated, deployment_group_updated, state) do
    handle_event(
      :deployment_group_updated,
      build_deployment_group(state, deployment_group_updated),
      state
    )
  end

  defp handle_event(:deployment_group_deleted, deployment_group_deleted, state) do
    if deployment_group =
         Enum.find(state.deployment_groups, &(&1.id == deployment_group_deleted.id)) do
      Teams.Broadcasts.deployment_group_deleted(deployment_group)
      remove_deployment_group(state, deployment_group)
    else
      state
    end
  end

  defp handle_event(:user_connected, connected, state) do
    state
    |> dispatch_secrets(connected)
    |> dispatch_file_systems(connected)
    |> dispatch_deployment_groups(connected)
  end

  defp handle_event(:agent_connected, agent_connected, state) do
    %{state | deployment_group_id: to_string(agent_connected.deployment_group_id)}
    |> update_hub(agent_connected)
    |> dispatch_secrets(agent_connected)
    |> dispatch_file_systems(agent_connected)
    |> dispatch_deployment_groups(agent_connected)
  end

  defp handle_event(:agent_key_created, agent_key_created, state) do
    agent_key = build_agent_key(agent_key_created)

    case find_deployment_group(agent_key.deployment_group_id, state.deployment_groups) do
      nil ->
        state

      deployment_group ->
        agent_keys = Enum.uniq([agent_key | deployment_group.agent_keys])
        deployment_group = %{deployment_group | agent_keys: Enum.reverse(agent_keys)}
        handle_event(:deployment_group_updated, deployment_group, state)
    end
  end

  defp handle_event(:agent_key_deleted, agent_key_deleted, state) do
    agent_key = build_agent_key(agent_key_deleted)

    case find_deployment_group(agent_key.deployment_group_id, state.deployment_groups) do
      nil ->
        state

      deployment_group ->
        agent_keys = Enum.reject(deployment_group.agent_keys, &(&1 == agent_key))
        deployment_group = %{deployment_group | agent_keys: agent_keys}
        handle_event(:deployment_group_updated, deployment_group, state)
    end
  end

  defp dispatch_secrets(state, %{secrets: secrets}) do
    decrypted_secrets = Enum.map(secrets, &build_secret(state, &1))

    {created, deleted, updated} =
      diff(
        state.secrets,
        decrypted_secrets,
        &(&1.name == &2.name and &1.value == &2.value),
        &(&1.name == &2.name),
        &(&1.name == &2.name and &1.value != &2.value)
      )

    dispatch_events(state,
      secret_deleted: deleted,
      secret_created: created,
      secret_updated: updated
    )
  end

  defp dispatch_file_systems(state, %{file_systems: file_systems}) do
    decrypted_file_systems = Enum.map(file_systems, &build_file_system(state, &1))

    {created, deleted, updated} =
      diff(
        state.file_systems,
        decrypted_file_systems,
        &(&1.external_id == &2.external_id)
      )

    dispatch_events(state,
      file_system_deleted: deleted,
      file_system_created: created,
      file_system_updated: updated
    )
  end

  defp dispatch_file_systems(state, _), do: state

  defp dispatch_deployment_groups(state, %{deployment_groups: deployment_groups}) do
    decrypted_deployment_groups = Enum.map(deployment_groups, &build_deployment_group(state, &1))

    {created, deleted, updated} =
      diff(state.deployment_groups, decrypted_deployment_groups, &(&1.id == &2.id))

    dispatch_events(state,
      deployment_group_deleted: deleted,
      deployment_group_created: created,
      deployment_group_updated: updated
    )
  end

  defp update_hub(state, %{public_key: org_public_key}) do
    hub = %{state.hub | org_public_key: org_public_key}

    if Livebook.Hubs.hub_exists?(hub.id) do
      Hubs.save_hub(hub)
    end

    %{state | hub: hub}
  end

  defp diff(old_list, new_list, fun, deleted_fun \\ nil, updated_fun \\ nil) do
    deleted_fun = unless deleted_fun, do: fun, else: deleted_fun
    updated_fun = unless updated_fun, do: fun, else: updated_fun

    created = Enum.reject(new_list, fn item -> Enum.find(old_list, &fun.(&1, item)) end)
    deleted = Enum.reject(old_list, fn item -> Enum.find(new_list, &deleted_fun.(&1, item)) end)
    updated = Enum.filter(new_list, fn item -> Enum.find(old_list, &updated_fun.(&1, item)) end)

    {created, deleted, updated}
  end

  defp dispatch_events(state, events_by_topic) do
    for {topic, events} <- events_by_topic,
        event <- events,
        reduce: state,
        do: (acc -> handle_event(topic, event, acc))
  end

  defp find_deployment_group(nil, _), do: nil
  defp find_deployment_group(id, groups), do: Enum.find(groups, &(&1.id == id))
end
