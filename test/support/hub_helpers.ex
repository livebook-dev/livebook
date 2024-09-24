defmodule Livebook.HubHelpers do
  import ExUnit.Assertions
  import Livebook.Factory
  import Phoenix.LiveViewTest

  @offline_hub_key "lb_tk_A9TarFeAzmX3sDwSPm5JP5qbLPnNpLpzmjVZUCHXwmI"
  @offline_hub_org_public_key "lb_opk_MIIBCgKCAQEAnDlKJkOnegStai82RYoJ95mFxsD__Vn86CJtyFyGi71dxUhyY1Xta779wNZbLuK7UGFFF18jjwWcGUohoxvkxykSVaC71ULeTX03zGFMQ61za77N9F9uCbwwEcngN7y8KfCAXWK4WAx5gA_a_LvdKmzAk3emWFvCKeyABXVnafj4HCUFPPdTdARFfIS56UvqXZ27Fbzbvk1DxAKR6js9kvQJDiDUD6R1dcv_Yu-Eh4iMjBZd0i1eGD9UntmVRhTZbwP70iP-i4Dv5z0573lVNyPdIwAVon7K4n-j8l7EIYwCQRtyZCHxsqkf_3f3VAiDK4ZLovGguutLC1tYObyXaQIDAQAB"
  @offline_hub_org_private_key "MIIEpAIBAAKCAQEAnDlKJkOnegStai82RYoJ95mFxsD__Vn86CJtyFyGi71dxUhyY1Xta779wNZbLuK7UGFFF18jjwWcGUohoxvkxykSVaC71ULeTX03zGFMQ61za77N9F9uCbwwEcngN7y8KfCAXWK4WAx5gA_a_LvdKmzAk3emWFvCKeyABXVnafj4HCUFPPdTdARFfIS56UvqXZ27Fbzbvk1DxAKR6js9kvQJDiDUD6R1dcv_Yu-Eh4iMjBZd0i1eGD9UntmVRhTZbwP70iP-i4Dv5z0573lVNyPdIwAVon7K4n-j8l7EIYwCQRtyZCHxsqkf_3f3VAiDK4ZLovGguutLC1tYObyXaQIDAQABAoIBAAhtzuJhpBehSPoBshvuZrtFPUKMB0PUJyEfOm0lEN1ZSkXqssFJUZYOqAJPjnvpH9ImbWPlbjW5R8LVjRsP1jgorySPl5LaGMR1jR0p4sOECEY39UTKIVXFIZLUIZTgSga5QzPGr8uQYL3YHSilujkfxQQv4HnD-aXpbL7epsXA44w7RlibwNhrNnMMYSCiRgTB8tqfuVtXAIaLfS46_v3WQqQ-al2yj3DRtxD2FmOfZzbUFkRXC9tUdCLwsNmrwhA06PYduCabZzZ8I-_j36OdCzMPSx9cVMMstDo11YP6yOIcUuloQO8iNBRrcXB7katt-PiJraJm-PVd_NPA8EECgYEAzHkJ0RaANfKUEo_FbDclURm_-uWjP-z2avwAMbjDL37S635BMHS5WuFH8YP6wTC3C7sx_RW2HFlZwhvzCko3yeeM9kvdR8OEoKcGi-Ngj6My3ks2Qy5AOoIDhPBzoI1HJU1kA5SdV81i9LTQk-D7CMHt_DxT5FwKP3L3XOMumFcCgYEAw5ebmKW77t5ksni0k2oV5l3uk0Jyg9lr_y9kkUytiosPtIxSayBag79KuR221lQVEAC5unqiGjBlCCE_doqWtXfmuEYVB43PSnsMOVbTTbk_2cR-qNmkM7407LJrvO6Zpru83HDhrjYiBucRiM_wXPJPA6Ko32fQAW9YPT07dj8CgYAwlLV7YyA1MRxzSIt8iaGpIjgV0Ye3AYMOqi8VoTNmzngokYfFjoYXjJz-SgBC9GMZO3HGEumA1M3Zq7BUCow0wXohbqb1jQOu8-A_Tle76OeGH1KWJaAHBqr1Y_fk9own1bpki2PS366aO3evGu4qB4GWw3KfOCsLJjKVdDi24wKBgQCVIJLR4AtaJNZB_SYw_0GTUysDvDXzsWJWPpw-7GekqkJfNl3gr9pTeRZP7gfpglJM0UDKnZXawetGN5Nbnm8qDTEsbsK577WM5CR902VobUXxk4--zbIUgYF4ttDOTF16csmcibSIT13CRYto9KIfO-BitTJso4pEjdCJYJZloQKBgQCJMlaq0riNXpsbkt5yFXy8_thXvCCEm_HEjdSbYB_UNhnnvvrK1X310LvXyRAY-GeLTAxjHm3fJlfccYB6yAslsBYCxLql0Yfszn9fltIBZQuisYajIzSo56p06Di7zZDXalHaCHfeGUCgDvlPt0UO4kWfSSd_o11wkLZBUnuLHA"
  @offline_hub_org_name "org-number-3079"

  @offline_hub %Livebook.Hubs.Team{
    id: "team-#{@offline_hub_org_name}",
    teams_key: @offline_hub_key,
    org_public_key: @offline_hub_org_public_key,
    hub_name: @offline_hub_org_name,
    user_id: nil,
    org_id: nil,
    org_key_id: nil,
    session_token: "",
    offline: %Livebook.Hubs.Team.Offline{
      secrets: []
    }
  }

  def create_team_hub(user, node) do
    hub = build_team_hub(user, node)
    Livebook.Hubs.save_hub(hub)
  end

  def create_agent_team_hub(node) do
    {agent_key, org, deployment_group, hub} = build_agent_team_hub(node)
    erpc_call(node, :create_org_key_pair, [[org: org]])
    ^hub = Livebook.Hubs.save_hub(hub)

    {agent_key, org, deployment_group, hub}
  end

  def build_team_headers(user, node) do
    hub = build_team_hub(user, node)

    headers = [
      {"x-user", to_string(hub.user_id)},
      {"x-org", to_string(hub.org_id)},
      {"x-org-key", to_string(hub.org_key_id)},
      {"x-session-token", hub.session_token}
    ]

    {hub, headers}
  end

  def build_team_hub(user, node) do
    teams_org = build(:org)
    teams_key = teams_org.teams_key
    key_hash = Livebook.Teams.Org.key_hash(teams_org)

    org = erpc_call(node, :create_org, [])
    org_key = erpc_call(node, :create_org_key, [[org: org, key_hash: key_hash]])
    org_key_pair = erpc_call(node, :create_org_key_pair, [[org: org]])
    token = erpc_call(node, :associate_user_with_org, [user, org])

    build(:team,
      id: "team-#{org.name}",
      hub_name: org.name,
      user_id: user.id,
      org_id: org.id,
      org_key_id: org_key.id,
      org_public_key: org_key_pair.public_key,
      session_token: token,
      teams_key: teams_key
    )
  end

  def build_agent_team_hub(node) do
    teams_org = build(:org)
    teams_key = teams_org.teams_key
    key_hash = Livebook.Teams.Org.key_hash(teams_org)

    org = erpc_call(node, :create_org, [])
    org_key = erpc_call(node, :create_org_key, [[org: org, key_hash: key_hash]])

    deployment_group =
      erpc_call(node, :create_deployment_group, [
        [
          name: "sleepy-cat-#{Ecto.UUID.generate()}",
          mode: :online,
          org: org
        ]
      ])

    agent_key = erpc_call(node, :create_agent_key, [[deployment_group: deployment_group]])

    team =
      build(:team,
        id: "team-#{org.name}",
        hub_name: org.name,
        user_id: nil,
        org_id: org.id,
        org_key_id: org_key.id,
        org_public_key: nil,
        session_token: agent_key.key,
        teams_key: teams_key
      )

    {agent_key, org, deployment_group, team}
  end

  def build_offline_team_hub(user, node) do
    teams_org = build(:org, teams_key: @offline_hub_key, name: @offline_hub_org_name)
    key_hash = Livebook.Teams.Org.key_hash(teams_org)

    org = erpc_call(node, :create_org, [[name: teams_org.name]])
    org_key = erpc_call(node, :create_org_key, [[org: org, key_hash: key_hash]])

    org_key_pair =
      erpc_call(node, :create_org_key_pair, [
        [
          org: org,
          public_key: @offline_hub_org_public_key,
          private_key: @offline_hub_org_private_key
        ]
      ])

    token = erpc_call(node, :associate_user_with_org, [user, org])

    build(:team,
      id: "team-#{org.name}",
      hub_name: org.name,
      user_id: user.id,
      org_id: org.id,
      org_key_id: org_key.id,
      org_public_key: org_key_pair.public_key,
      session_token: token,
      teams_key: teams_org.teams_key
    )
  end

  def assert_sidebar_hub(view, id, name, emoji \\ "🐈") do
    hub = element(view, hub_element_id(id))
    hub_html = render(hub)

    assert hub_html =~ emoji
    assert hub_html =~ "/hub/#{id}"
    assert hub_html =~ name
  end

  def refute_sidebar_hub(view, id) do
    refute has_element?(view, hub_element_id(id))
  end

  def set_offline_hub() do
    hub = offline_hub()
    ^hub = Livebook.Hubs.save_hub(hub)

    hub
  end

  def offline_hub(), do: @offline_hub

  def put_offline_hub_secret(secret) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)
    secret_key = Livebook.Teams.derive_key(hub.teams_key)
    value = Livebook.Teams.encrypt(secret.value, secret_key)
    secret_created = %LivebookProto.SecretCreated{name: secret.name, value: value}

    send(pid, {:event, :secret_created, secret_created})
  end

  def remove_offline_hub_secret(secret) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)
    secret_deleted = %LivebookProto.SecretDeleted{name: secret.name}

    send(pid, {:event, :secret_deleted, secret_deleted})
  end

  def put_offline_hub_deployment_group(deployment_group) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)

    deployment_group_created =
      %LivebookProto.DeploymentGroupCreated{
        id: deployment_group.id,
        name: deployment_group.name,
        mode: deployment_group.mode
      }

    send(pid, {:event, :deployment_group_created, deployment_group_created})
  end

  def remove_offline_hub_deployment_group(deployment_group) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)

    deployment_group_deleted =
      %LivebookProto.DeploymentGroupDeleted{id: deployment_group.id}

    send(pid, {:event, :deployment_group_deleted, deployment_group_deleted})
  end

  def put_offline_hub_file_system(file_system) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)
    secret_key = Livebook.Teams.derive_key(hub.teams_key)
    %{name: name} = Livebook.FileSystem.external_metadata(file_system)
    attrs = Livebook.FileSystem.dump(file_system)
    json = Jason.encode!(attrs)
    value = Livebook.Teams.encrypt(json, secret_key)

    file_system_created =
      %LivebookProto.FileSystemCreated{
        id: file_system.external_id,
        name: name,
        type: Livebook.FileSystems.type(file_system),
        value: value
      }

    send(pid, {:event, :file_system_created, file_system_created})
  end

  def remove_offline_hub_file_system(file_system) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)
    file_system_deleted = %LivebookProto.FileSystemDeleted{id: file_system.external_id}

    send(pid, {:event, :file_system_deleted, file_system_deleted})
  end

  def create_teams_file_system(hub, node, org_key \\ nil) do
    org_key = if org_key, do: org_key, else: erpc_call(node, :get_org_key!, [hub.org_key_id])
    erpc_call(node, :create_file_system, [[org_key: org_key]])
  end

  def build_bypass_file_system(bypass, hub_id \\ Livebook.Hubs.Personal.id()) do
    bucket_url = "http://localhost:#{bypass.port}/mybucket"

    file_system =
      build(:fs_s3,
        id: Livebook.FileSystem.S3.id(hub_id, bucket_url),
        bucket_url: bucket_url,
        region: "auto",
        hub_id: hub_id
      )

    file_system
  end

  def persist_file_system(file_system) do
    hub = Livebook.Hubs.fetch_hub!(Livebook.Hubs.Personal.id())
    :ok = Livebook.Hubs.create_file_system(hub, file_system)
  end

  def erpc_call(node, fun, args) do
    :erpc.call(node, TeamsRPC, fun, args)
  end

  def simulate_agent_join(hub, deployment_group) do
    Livebook.Teams.Broadcasts.subscribe([:agents])

    # Simulates the agent join event
    pid = Livebook.Hubs.TeamClient.get_pid(hub.id)

    agent =
      build(:agent,
        hub_id: hub.id,
        org_id: to_string(hub.org_id),
        deployment_group_id: to_string(deployment_group.id)
      )

    livebook_proto_agent =
      %LivebookProto.Agent{
        id: agent.id,
        name: agent.name,
        org_id: agent.org_id,
        deployment_group_id: agent.deployment_group_id
      }

    livebook_proto_agent_joined = %LivebookProto.AgentJoined{agent: livebook_proto_agent}
    send(pid, {:event, :agent_joined, livebook_proto_agent_joined})

    assert_receive {:agent_joined, ^agent}
  end

  @doc """
  Creates a new Team hub from given user and node, and await the WebSocket to be connected.

      test "my test", %{user: user, node: node} do
        team = connect_to_teams(user, node)
        assert "team-" <> _ = team.id
      end

  """
  @spec connect_to_teams(struct(), node()) :: Livebook.Hubs.Team.t()
  def connect_to_teams(user, node) do
    %{id: id} = team = create_team_hub(user, node)
    assert_receive {:hub_connected, ^id}, 3_000
    assert_receive {:client_connected, ^id}, 3_000

    team
  end

  defp hub_pid(hub) do
    if pid = GenServer.whereis({:via, Registry, {Livebook.HubsRegistry, hub.id}}) do
      {:ok, pid}
    end
  end

  defp hub_element_id(id), do: "#hubs #hub-#{id}"
end
