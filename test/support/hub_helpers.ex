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
    user_id: 0,
    org_id: 0,
    org_key_id: 0,
    session_token: "",
    offline: %Livebook.Hubs.Team.Offline{
      secrets: []
    }
  }

  def create_team_hub(user, node) do
    hub = build_team_hub(user, node)
    Livebook.Hubs.save_hub(hub)
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
    secret_created = LivebookProto.SecretCreated.new(name: secret.name, value: value)

    send(pid, {:event, :secret_created, secret_created})
  end

  def remove_offline_hub_secret(secret) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)
    secret_deleted = LivebookProto.SecretDeleted.new(name: secret.name)

    send(pid, {:event, :secret_deleted, secret_deleted})
  end

  def put_offline_hub_deployment_group(deployment_group) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)

    deployment_group_created =
      LivebookProto.DeploymentGroupCreated.new(
        id: deployment_group.id,
        name: deployment_group.name,
        mode: deployment_group.mode
      )

    send(pid, {:event, :deployment_group_created, deployment_group_created})
  end

  def remove_offline_hub_deployment_group(deployment_group) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)

    deployment_group_deleted =
      LivebookProto.DeploymentGroupDeleted.new(id: deployment_group.id)

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
      LivebookProto.FileSystemCreated.new(
        id: file_system.external_id,
        name: name,
        type: Livebook.FileSystems.type(file_system),
        value: value
      )

    send(pid, {:event, :file_system_created, file_system_created})
  end

  def remove_offline_hub_file_system(file_system) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)
    file_system_deleted = LivebookProto.FileSystemDeleted.new(id: file_system.external_id)

    send(pid, {:event, :file_system_deleted, file_system_deleted})
  end

  def create_teams_file_system(hub, node) do
    org_key = erpc_call(node, :get_org_key!, [hub.org_key_id])
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

  defp hub_pid(hub) do
    if pid = GenServer.whereis({:via, Registry, {Livebook.HubsRegistry, hub.id}}) do
      {:ok, pid}
    end
  end

  defp hub_element_id(id), do: "#hubs #hub-#{id}"

  defp erpc_call(node, fun, args) do
    :erpc.call(node, TeamsRPC.Integration, fun, args)
  end
end
