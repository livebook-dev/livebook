defmodule Livebook.Hubs.TeamClientTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs.TeamClient

  setup do
    Livebook.Hubs.Broadcasts.subscribe([:connection, :file_systems, :secrets])
    Livebook.Teams.Broadcasts.subscribe([:deployment_groups, :app_deployments, :agents])
    :ok
  end

  test "rejects the web socket connection with invalid credentials", %{user: user, token: token} do
    team =
      build(:team,
        user_id: user.id,
        org_id: 123_456,
        org_key_id: 123_456,
        session_token: token
      )

    id = team.id

    TeamClient.start_link(team)

    assert_receive {:hub_server_error, ^id, error}

    assert error ==
             "#{team.hub_name}: Your session is out-of-date. Please re-join the organization."

    refute Livebook.Hubs.hub_exists?(team.id)
  end

  describe "handle events" do
    setup %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      assert_receive {:hub_connected, ^id}

      {:ok, team: team}
    end

    test "receives secret events", %{team: team} do
      secret = build(:secret, name: "SECRET", value: "BAR")
      assert Livebook.Hubs.create_secret(team, secret) == :ok

      name = secret.name
      value = secret.value

      # receives `{:secret_created, secret}` event
      # with the value decrypted
      assert_receive {:secret_created, %{name: ^name, value: ^value}}

      # updates the secret
      updated_secret = Map.replace!(secret, :value, "BAZ")
      assert Livebook.Hubs.update_secret(team, updated_secret) == :ok

      new_value = updated_secret.value

      # receives `{:secret_updated, secret}` event
      # with the value decrypted
      assert_receive {:secret_updated, %{name: ^name, value: ^new_value}}

      # deletes the secret
      assert Livebook.Hubs.delete_secret(team, updated_secret) == :ok

      # receives `{:secret_deleted, secret}` event
      assert_receive {:secret_deleted, %{name: ^name, value: ^new_value}}
    end

    test "receives file system events", %{team: team} do
      file_system =
        build(:fs_s3, bucket_url: "https://file_system.s3.amazonaws.com", region: "us-east-1")

      assert Livebook.Hubs.create_file_system(team, file_system) == :ok

      bucket_url = file_system.bucket_url
      region = file_system.region

      # receives `{:file_system_created, file_system}` event
      assert_receive {:file_system_created,
                      %{external_id: id, bucket_url: ^bucket_url, region: ^region}}

      # updates the file system
      updated_file_system = %{file_system | region: "eu-central-1", external_id: id}
      assert Livebook.Hubs.update_file_system(team, updated_file_system) == :ok

      new_region = updated_file_system.region

      # receives `{:file_system_updated, file_system}` event
      assert_receive {:file_system_updated,
                      %{external_id: ^id, bucket_url: ^bucket_url, region: ^new_region}}

      # deletes the file system
      assert Livebook.Hubs.delete_file_system(team, updated_file_system) == :ok

      # receives `{:file_system_deleted, file_system}` event
      assert_receive {:file_system_deleted, %{external_id: ^id, bucket_url: ^bucket_url}}
    end

    test "receives deployment group events", %{team: team} do
      deployment_group =
        build(:deployment_group, name: "DEPLOYMENT_GROUP_#{team.id}", mode: :online)

      assert {:ok, _} = Livebook.Teams.create_deployment_group(team, deployment_group)
      %{name: name, mode: mode} = deployment_group

      # receives `{:event, :deployment_group_created, deployment_group}` event
      assert_receive {:deployment_group_created, %{name: ^name, mode: ^mode}}
    end

    @tag :tmp_dir
    test "receives app events", %{team: team, tmp_dir: tmp_dir} do
      deployment_group = build(:deployment_group, name: team.id, mode: :online)
      assert {:ok, id} = Livebook.Teams.create_deployment_group(team, deployment_group)

      id = to_string(id)

      assert_receive {:deployment_group_created, %{id: ^id}}

      # creates the app deployment
      slug = Livebook.Utils.random_short_id()
      title = "MyNotebook-#{slug}"

      notebook = %{
        Livebook.Notebook.new()
        | app_settings: %{Livebook.Notebook.AppSettings.new() | slug: slug},
          name: title,
          hub_id: team.id,
          deployment_group_id: id
      }

      files_dir = Livebook.FileSystem.File.local(tmp_dir)
      {:ok, app_deployment} = Livebook.Teams.AppDeployment.new(notebook, files_dir)
      :ok = Livebook.Teams.deploy_app(team, app_deployment)

      sha = app_deployment.sha

      assert_receive {:app_deployment_created,
                      %Livebook.Teams.AppDeployment{
                        slug: ^slug,
                        sha: ^sha,
                        title: ^title,
                        deployment_group_id: ^id
                      }}
    end
  end

  describe "handle user_connected event" do
    setup %{user: user, node: node} do
      team = build_team_hub(user, node)

      user_connected =
        %LivebookProto.UserConnected{
          name: team.hub_name,
          secrets: [],
          file_systems: [],
          deployment_groups: [],
          app_deployments: []
        }

      {:ok, team: team, user_connected: user_connected}
    end

    test "dispatches the secrets list", %{team: team, user_connected: user_connected} do
      secret =
        build(:secret,
          name: "CHONKY_CAT",
          value: "an encrypted value",
          hub_id: team.id
        )

      secret_key = Livebook.Teams.derive_key(team.teams_key)
      secret_value = Livebook.Teams.encrypt(secret.value, secret_key)
      livebook_proto_secret = %LivebookProto.Secret{name: secret.name, value: secret_value}

      # creates the secret
      user_connected = %{user_connected | secrets: [livebook_proto_secret]}
      pid = connect_to_teams(team)
      refute_receive {:secret_created, ^secret}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:secret_created, ^secret}
      assert secret in TeamClient.get_secrets(team.id)

      # updates the secret
      updated_secret = %{secret | value: "an updated value"}
      secret_value = Livebook.Teams.encrypt(updated_secret.value, secret_key)
      updated_livebook_proto_secret = %{livebook_proto_secret | value: secret_value}
      user_connected = %{user_connected | secrets: [updated_livebook_proto_secret]}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:secret_updated, ^updated_secret}
      refute secret in TeamClient.get_secrets(team.id)
      assert updated_secret in TeamClient.get_secrets(team.id)

      # deletes the secret
      user_connected = %{user_connected | secrets: []}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:secret_deleted, ^updated_secret}
      refute updated_secret in TeamClient.get_secrets(team.id)
    end

    test "dispatches the file systems list", %{team: team, user_connected: user_connected} do
      bucket_url = "https://mybucket.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      fs_id = "#{team.id}-s3-#{Base.url_encode64(hash, padding: false)}"

      file_system =
        build(:fs_s3, id: fs_id, bucket_url: bucket_url, external_id: "123456", hub_id: team.id)

      type = Livebook.FileSystems.type(file_system)
      %{name: name} = Livebook.FileSystem.external_metadata(file_system)
      attrs = Livebook.FileSystem.dump(file_system)
      credentials = Jason.encode!(attrs)

      secret_key = Livebook.Teams.derive_key(team.teams_key)
      value = Livebook.Teams.encrypt(credentials, secret_key)

      livebook_proto_file_system =
        %LivebookProto.FileSystem{
          id: file_system.external_id,
          name: name,
          type: to_string(type),
          value: value
        }

      # creates the file system
      user_connected = %{user_connected | file_systems: [livebook_proto_file_system]}
      pid = connect_to_teams(team)
      refute_receive {:file_system_created, ^file_system}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:file_system_created, ^file_system}
      assert file_system in TeamClient.get_file_systems(team.id)

      # updates the file system
      updated_file_system = %{
        file_system
        | id: "#{team.id}-s3-ATC52Lo7d-bS21OLjPQ8KFBPJN8ku4hCn2nic2jTGeI",
          bucket_url: "https://updated_name.s3.amazonaws.com"
      }

      updated_attrs = Livebook.FileSystem.dump(updated_file_system)
      updated_credentials = Jason.encode!(updated_attrs)
      updated_value = Livebook.Teams.encrypt(updated_credentials, secret_key)

      updated_livebook_proto_file_system = %{
        livebook_proto_file_system
        | name: updated_file_system.bucket_url,
          value: updated_value
      }

      user_connected = %{user_connected | file_systems: [updated_livebook_proto_file_system]}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:file_system_updated, ^updated_file_system}
      refute file_system in TeamClient.get_file_systems(team.id)
      assert updated_file_system in TeamClient.get_file_systems(team.id)

      # deletes the file system
      user_connected = %{user_connected | file_systems: []}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:file_system_deleted, ^updated_file_system}
      refute updated_file_system in TeamClient.get_file_systems(team.id)
    end

    test "dispatches the deployment groups list", %{team: team, user_connected: user_connected} do
      deployment_group =
        build(:deployment_group,
          id: "1",
          name: "sleepy-cat-#{System.unique_integer([:positive])}",
          mode: :offline,
          hub_id: team.id
        )

      livebook_proto_deployment_group =
        %LivebookProto.DeploymentGroup{
          id: to_string(deployment_group.id),
          name: deployment_group.name,
          mode: to_string(deployment_group.mode),
          secrets: [],
          agent_keys: []
        }

      # creates the deployment group
      user_connected = %{user_connected | deployment_groups: [livebook_proto_deployment_group]}
      pid = connect_to_teams(team)
      refute_receive {:deployment_group_created, ^deployment_group}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:deployment_group_created, ^deployment_group}
      assert deployment_group in TeamClient.get_deployment_groups(team.id)

      # updates the deployment group
      updated_deployment_group = %{deployment_group | mode: :online}

      updated_livebook_proto_deployment_group = %{
        livebook_proto_deployment_group
        | mode: to_string(updated_deployment_group.mode)
      }

      user_connected = %{
        user_connected
        | deployment_groups: [updated_livebook_proto_deployment_group]
      }

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:deployment_group_updated, ^updated_deployment_group}
      refute deployment_group in TeamClient.get_deployment_groups(team.id)
      assert updated_deployment_group in TeamClient.get_deployment_groups(team.id)

      # deletes the deployment group
      user_connected = %{user_connected | deployment_groups: []}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:deployment_group_deleted, ^updated_deployment_group}
      refute updated_deployment_group in TeamClient.get_deployment_groups(team.id)
    end

    test "dispatches the app deployments list", %{team: team, user_connected: user_connected} do
      pid = connect_to_teams(team)

      deployment_group =
        build(:deployment_group,
          id: "1",
          name: "sleepy-cat-#{System.unique_integer([:positive])}",
          mode: :online,
          hub_id: team.id
        )

      livebook_proto_deployment_group =
        %LivebookProto.DeploymentGroup{
          id: to_string(deployment_group.id),
          name: deployment_group.name,
          mode: to_string(deployment_group.mode),
          secrets: [],
          agent_keys: []
        }

      app_deployment =
        build(:app_deployment,
          hub_id: team.id,
          file: nil,
          deployment_group_id: deployment_group.id
        )

      {seconds, 0} = NaiveDateTime.to_gregorian_seconds(app_deployment.deployed_at)

      livebook_proto_app_deployment =
        %LivebookProto.AppDeployment{
          id: app_deployment.id,
          title: app_deployment.title,
          slug: app_deployment.slug,
          sha: app_deployment.sha,
          deployed_by: app_deployment.deployed_by,
          deployed_at: seconds,
          revision_id: "1",
          deployment_group_id: app_deployment.deployment_group_id
        }

      user_connected = %{
        user_connected
        | deployment_groups: [livebook_proto_deployment_group],
          app_deployments: [livebook_proto_app_deployment]
      }

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:app_deployment_created, ^app_deployment}, 5000
      assert app_deployment in TeamClient.get_app_deployments(team.id)
    end

    test "dispatches the agents list", %{team: team, user_connected: user_connected} do
      pid = connect_to_teams(team)
      agent = build(:agent, hub_id: team.id, org_id: to_string(team.org_id))

      livebook_proto_agent =
        %LivebookProto.Agent{
          id: agent.id,
          name: agent.name,
          org_id: agent.org_id,
          deployment_group_id: agent.deployment_group_id
        }

      user_connected = %{user_connected | agents: [livebook_proto_agent]}

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:agent_joined, ^agent}
      assert agent in TeamClient.get_agents(team.id)

      user_connected = %{user_connected | agents: []}

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:agent_left, ^agent}
      refute agent in TeamClient.get_agents(team.id)
    end
  end

  describe "handle agent_connected event" do
    setup %{node: node} do
      {agent_key, org, deployment_group, team} = build_agent_team_hub(node)
      org_key_pair = erpc_call(node, :create_org_key_pair, [[org: org]])

      agent_connected =
        %LivebookProto.AgentConnected{
          id: agent_key.id,
          name: Livebook.Config.agent_name(),
          public_key: org_key_pair.public_key,
          deployment_group_id: deployment_group.id,
          secrets: [],
          file_systems: [],
          deployment_groups: [],
          app_deployments: [],
          agents: []
        }

      {:ok,
       team: team,
       org: org,
       deployment_group: deployment_group,
       agent_connected: agent_connected,
       agent_key: agent_key}
    end

    test "dispatches the secrets list", %{team: team, agent_connected: agent_connected} do
      secret =
        build(:secret,
          name: "AGENT_SECRET",
          value: "an encrypted value",
          hub_id: team.id
        )

      secret_key = Livebook.Teams.derive_key(team.teams_key)
      secret_value = Livebook.Teams.encrypt(secret.value, secret_key)
      livebook_proto_secret = %LivebookProto.Secret{name: secret.name, value: secret_value}

      # creates the secret
      agent_connected = %{agent_connected | secrets: [livebook_proto_secret]}
      pid = connect_to_teams(team)
      refute_receive {:secret_created, ^secret}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:secret_created, ^secret}
      assert secret in TeamClient.get_secrets(team.id)

      # updates the secret
      updated_secret = %{secret | value: "an updated value"}
      secret_value = Livebook.Teams.encrypt(updated_secret.value, secret_key)
      updated_livebook_proto_secret = %{livebook_proto_secret | value: secret_value}
      agent_connected = %{agent_connected | secrets: [updated_livebook_proto_secret]}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:secret_updated, ^updated_secret}
      refute secret in TeamClient.get_secrets(team.id)
      assert updated_secret in TeamClient.get_secrets(team.id)

      # deletes the secret
      agent_connected = %{agent_connected | secrets: []}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:secret_deleted, ^updated_secret}
      refute updated_secret in TeamClient.get_secrets(team.id)
    end

    test "dispatches the file systems list", %{team: team, agent_connected: agent_connected} do
      bucket_url = "https://mybucket.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      fs_id = "#{team.id}-s3-#{Base.url_encode64(hash, padding: false)}"

      file_system =
        build(:fs_s3, id: fs_id, bucket_url: bucket_url, external_id: "123456", hub_id: team.id)

      type = Livebook.FileSystems.type(file_system)
      %{name: name} = Livebook.FileSystem.external_metadata(file_system)
      attrs = Livebook.FileSystem.dump(file_system)
      credentials = Jason.encode!(attrs)

      secret_key = Livebook.Teams.derive_key(team.teams_key)
      value = Livebook.Teams.encrypt(credentials, secret_key)

      livebook_proto_file_system =
        %LivebookProto.FileSystem{
          id: file_system.external_id,
          name: name,
          type: to_string(type),
          value: value
        }

      # creates the file system
      agent_connected = %{agent_connected | file_systems: [livebook_proto_file_system]}
      pid = connect_to_teams(team)
      refute_receive {:file_system_created, ^file_system}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:file_system_created, ^file_system}
      assert file_system in TeamClient.get_file_systems(team.id)

      # updates the file system
      updated_file_system = %{
        file_system
        | id: "#{team.id}-s3-ATC52Lo7d-bS21OLjPQ8KFBPJN8ku4hCn2nic2jTGeI",
          bucket_url: "https://updated_name.s3.amazonaws.com"
      }

      updated_attrs = Livebook.FileSystem.dump(updated_file_system)
      updated_credentials = Jason.encode!(updated_attrs)
      updated_value = Livebook.Teams.encrypt(updated_credentials, secret_key)

      updated_livebook_proto_file_system = %{
        livebook_proto_file_system
        | name: updated_file_system.bucket_url,
          value: updated_value
      }

      agent_connected = %{agent_connected | file_systems: [updated_livebook_proto_file_system]}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:file_system_updated, ^updated_file_system}
      refute file_system in TeamClient.get_file_systems(team.id)
      assert updated_file_system in TeamClient.get_file_systems(team.id)

      # deletes the file system
      agent_connected = %{agent_connected | file_systems: []}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:file_system_deleted, ^updated_file_system}
      refute updated_file_system in TeamClient.get_file_systems(team.id)
    end

    test "dispatches the deployment groups list",
         %{team: team, deployment_group: teams_deployment_group, agent_connected: agent_connected} do
      deployment_group =
        build(:deployment_group,
          id: to_string(teams_deployment_group.id),
          name: teams_deployment_group.name,
          mode: teams_deployment_group.mode,
          hub_id: team.id,
          secrets: []
        )

      livebook_proto_deployment_group =
        %LivebookProto.DeploymentGroup{
          id: to_string(deployment_group.id),
          name: deployment_group.name,
          mode: to_string(deployment_group.mode),
          secrets: []
        }

      # creates the deployment group
      agent_connected = %{agent_connected | deployment_groups: [livebook_proto_deployment_group]}
      pid = connect_to_teams(team)
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:deployment_group_created, ^deployment_group}
      assert deployment_group in TeamClient.get_deployment_groups(team.id)

      # updates the deployment group
      updated_livebook_proto_deployment_group = %{
        livebook_proto_deployment_group
        | name: "A WHOLE NEW NAME"
      }

      agent_connected = %{
        agent_connected
        | deployment_groups: [updated_livebook_proto_deployment_group]
      }

      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:deployment_group_updated, %{name: "A WHOLE NEW NAME"}}
      refute deployment_group in TeamClient.get_deployment_groups(team.id)

      assert Enum.find(
               TeamClient.get_deployment_groups(team.id),
               &(&1.name == "A WHOLE NEW NAME")
             )

      # deletes the deployment group
      agent_connected = %{agent_connected | deployment_groups: []}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:deployment_group_deleted, %{name: "A WHOLE NEW NAME"}}

      refute Enum.find(
               TeamClient.get_deployment_groups(team.id),
               &(&1.name == "A WHOLE NEW NAME")
             )
    end

    test "dispatches the secrets list and override with deployment group secret",
         %{team: team, deployment_group: teams_deployment_group, agent_connected: agent_connected} do
      secret =
        build(:secret,
          name: "ORG_SECRET",
          value: "an encrypted value",
          hub_id: team.id
        )

      secret_key = Livebook.Teams.derive_key(team.teams_key)
      secret_value = Livebook.Teams.encrypt(secret.value, secret_key)
      livebook_proto_secret = %LivebookProto.Secret{name: secret.name, value: secret_value}

      # creates the secret
      agent_connected = %{agent_connected | secrets: [livebook_proto_secret]}
      pid = connect_to_teams(team)
      refute_receive {:secret_created, ^secret}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:secret_created, ^secret}
      assert secret in TeamClient.get_secrets(team.id)

      # overrides the secret with deployment group secret
      override_secret = %{
        secret
        | value: "an updated value",
          deployment_group_id: teams_deployment_group.id
      }

      secret_value = Livebook.Teams.encrypt(override_secret.value, secret_key)

      livebook_proto_deployment_group_secret =
        %LivebookProto.DeploymentGroupSecret{
          name: override_secret.name,
          value: secret_value,
          deployment_group_id: override_secret.deployment_group_id
        }

      deployment_group =
        build(:deployment_group,
          id: to_string(teams_deployment_group.id),
          name: teams_deployment_group.name,
          mode: teams_deployment_group.mode,
          hub_id: team.id,
          secrets: [override_secret]
        )

      livebook_proto_deployment_group =
        %LivebookProto.DeploymentGroup{
          id: to_string(deployment_group.id),
          name: deployment_group.name,
          mode: to_string(deployment_group.mode),
          secrets: [livebook_proto_deployment_group_secret]
        }

      agent_connected = %{agent_connected | deployment_groups: [livebook_proto_deployment_group]}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:deployment_group_created, ^deployment_group}
      refute secret in TeamClient.get_secrets(team.id)
      assert override_secret in TeamClient.get_secrets(team.id)
    end

    @tag :tmp_dir
    test "dispatches the app deployments list",
         %{
           team: team,
           org: teams_org,
           deployment_group: teams_deployment_group,
           agent_key: teams_agent_key,
           agent_connected: agent_connected,
           tmp_dir: tmp_dir,
           node: node
         } do
      agent_key =
        build(:agent_key,
          id: to_string(teams_agent_key.id),
          key: teams_agent_key.key,
          deployment_group_id: to_string(teams_agent_key.deployment_group_id)
        )

      deployment_group =
        build(:deployment_group,
          id: to_string(teams_deployment_group.id),
          name: teams_deployment_group.name,
          mode: teams_deployment_group.mode,
          hub_id: team.id,
          agent_keys: [agent_key]
        )

      livebook_proto_agent_key =
        %LivebookProto.AgentKey{
          id: agent_key.id,
          key: agent_key.key,
          deployment_group_id: agent_key.deployment_group_id
        }

      livebook_proto_deployment_group =
        %LivebookProto.DeploymentGroup{
          id: to_string(deployment_group.id),
          name: deployment_group.name,
          mode: to_string(deployment_group.mode),
          secrets: [],
          agent_keys: [livebook_proto_agent_key]
        }

      agent_connected = %{agent_connected | deployment_groups: [livebook_proto_deployment_group]}

      pid = connect_to_teams(team)

      # Since we're connecting as Agent, we should receive the
      # `:deployment_group_created` event from `:agent_connected` event
      assert_receive {:deployment_group_created, ^deployment_group}
      assert deployment_group in TeamClient.get_deployment_groups(team.id)

      # creates a new app deployment
      deployment_group_id = to_string(deployment_group.id)
      slug = Livebook.Utils.random_short_id()
      title = "MyNotebook2-#{slug}"

      notebook = %{
        Livebook.Notebook.new()
        | app_settings: %{Livebook.Notebook.AppSettings.new() | slug: slug},
          name: title,
          hub_id: team.id,
          deployment_group_id: deployment_group_id
      }

      files_dir = Livebook.FileSystem.File.local(tmp_dir)

      {:ok, %Livebook.Teams.AppDeployment{file: zip_content} = app_deployment} =
        Livebook.Teams.AppDeployment.new(notebook, files_dir)

      secret_key = Livebook.Teams.derive_key(team.teams_key)
      encrypted_content = Livebook.Teams.encrypt(zip_content, secret_key)

      teams_app_deployment =
        erpc_call(node, :upload_app_deployment, [
          teams_org,
          teams_deployment_group,
          app_deployment,
          encrypted_content
        ])

      # Since the app deployment struct generation is from Livebook side,
      # we don't have yet the information about who deployed the app,
      # so we need to add it ourselves
      app_deployment = %{
        app_deployment
        | id: to_string(teams_app_deployment.id),
          file: nil,
          deployed_by: teams_app_deployment.app_revision.created_by.name,
          deployed_at: teams_app_deployment.updated_at
      }

      {seconds, 0} = NaiveDateTime.to_gregorian_seconds(app_deployment.deployed_at)

      livebook_proto_app_deployment =
        %LivebookProto.AppDeployment{
          id: app_deployment.id,
          title: app_deployment.title,
          slug: app_deployment.slug,
          sha: app_deployment.sha,
          deployed_by: app_deployment.deployed_by,
          deployed_at: seconds,
          revision_id: to_string(teams_app_deployment.app_revision.id),
          deployment_group_id: app_deployment.deployment_group_id
        }

      agent_connected = %{agent_connected | app_deployments: [livebook_proto_app_deployment]}

      Livebook.Apps.subscribe()

      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:app_deployment_created, ^app_deployment}
      assert_receive {:app_created, %{pid: app_pid, slug: ^slug}}

      assert_receive {:app_updated,
                      %{slug: ^slug, sessions: [%{app_status: %{execution: :executed}}]}}

      assert app_deployment in TeamClient.get_app_deployments(team.id)

      Livebook.Hubs.delete_hub(team.id)
      Livebook.App.close(app_pid)
    end

    test "dispatches the agents list", %{team: team, agent_connected: agent_connected} do
      pid = connect_to_teams(team)

      # Since we're connecting as Agent, we should receive the
      # `:agent_joined` event from `:agent_connected` event
      assert_receive {:agent_joined, agent}
      assert agent in TeamClient.get_agents(team.id)

      assert_receive {:deployment_group_created, deployment_group}

      livebook_proto_deployment_group =
        %LivebookProto.DeploymentGroup{
          id: to_string(deployment_group.id),
          name: deployment_group.name,
          mode: to_string(deployment_group.mode),
          secrets: [],
          agent_keys: []
        }

      agent_connected = %{
        agent_connected
        | deployment_groups: [livebook_proto_deployment_group],
          agents: []
      }

      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:agent_left, ^agent}
      refute agent in TeamClient.get_agents(team.id)
    end
  end

  defp connect_to_teams(%{id: id} = team) do
    Livebook.Hubs.save_hub(team)
    assert_receive {:hub_connected, ^id}
    TeamClient.get_pid(team.id)
  end
end
