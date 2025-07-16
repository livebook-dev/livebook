defmodule Livebook.Hubs.TeamClientTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs.TeamClient

  setup :teams

  @moduletag subscribe_to_hubs_topics: [:crud, :connection, :file_systems, :secrets]
  @moduletag subscribe_to_teams_topics: [:clients, :deployment_groups, :app_deployments, :agents]

  describe "connect" do
    @describetag teams_for: :user
    @describetag teams_persisted: false

    test "successfully authenticates the websocket connection", %{team: team} do
      id = team.id

      assert {:ok, _pid} = TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
      assert_receive {:client_connected, ^id}

      TeamClient.stop(id)
    end

    @tag capture_log: true
    test "rejects the web socket connection with invalid credentials", %{team: team} do
      team = %{team | org_id: 123_456, org_key_id: 123_456}
      id = team.id

      error =
        "#{team.hub_name}: Your session is out-of-date. Please re-join the organization."

      start_supervised!({TeamClient, team})
      assert_receive {:hub_server_error, ^id, ^error}
    end
  end

  describe "handle user_connected event" do
    @describetag teams_for: :user

    setup %{team: team} do
      user_connected =
        %LivebookProto.UserConnected{
          name: team.hub_name,
          secrets: [],
          file_systems: [],
          deployment_groups: [],
          app_deployments: []
        }

      pid = TeamClient.get_pid(team.id)

      {:ok, pid: pid, user_connected: user_connected}
    end

    test "receives the user events", %{team: team, node: node} do
      # force user to be deleted from org
      TeamsRPC.delete_user_org(node, team.user_id, team.org_id)

      id = team.id
      reason = "#{team.hub_name}: you were removed from the org"

      assert_receive {:hub_server_error, ^id, ^reason}
      assert_receive {:hub_deleted, ^id}
      refute team in Livebook.Hubs.get_hubs()
    end

    test "dispatches the secrets list", %{team: team, pid: pid, user_connected: user_connected} do
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
      refute_received {:secret_created, ^secret}
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

    test "dispatches the file systems list",
         %{team: team, pid: pid, user_connected: user_connected} do
      bucket_url = "https://mybucket.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      fs_id = "#{team.id}-s3-#{Base.url_encode64(hash, padding: false)}"

      file_system =
        build(:fs_s3, id: fs_id, bucket_url: bucket_url, external_id: "123456", hub_id: team.id)

      type = Livebook.FileSystems.type(file_system)
      %{name: name} = Livebook.FileSystem.external_metadata(file_system)
      attrs = Livebook.FileSystem.dump(file_system)
      credentials = JSON.encode!(attrs)

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
      refute_received {:file_system_created, ^file_system}
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
      updated_credentials = JSON.encode!(updated_attrs)
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

    test "dispatches the deployment groups list",
         %{team: team, pid: pid, user_connected: user_connected} do
      deployment_group =
        build(:deployment_group,
          id: "1",
          name: "sleepy-cat-#{System.unique_integer([:positive])}",
          mode: :offline,
          hub_id: team.id,
          teams_auth: false
        )

      livebook_proto_deployment_group =
        %LivebookProto.DeploymentGroup{
          id: to_string(deployment_group.id),
          name: deployment_group.name,
          mode: to_string(deployment_group.mode),
          secrets: [],
          agent_keys: [],
          teams_auth: deployment_group.teams_auth
        }

      # creates the deployment group
      user_connected = %{user_connected | deployment_groups: [livebook_proto_deployment_group]}
      refute_received {:deployment_group_created, ^deployment_group}
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

    test "dispatches the app deployments list",
         %{team: team, pid: pid, user_connected: user_connected} do
      hub_id = team.id

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
          version: app_deployment.version,
          slug: app_deployment.slug,
          sha: app_deployment.sha,
          deployed_by: app_deployment.deployed_by,
          deployed_at: seconds,
          revision_id: "1",
          deployment_group_id: app_deployment.deployment_group_id,
          multi_session: app_deployment.multi_session,
          access_type: to_string(app_deployment.access_type)
        }

      user_connected = %{
        user_connected
        | deployment_groups: [livebook_proto_deployment_group],
          app_deployments: [livebook_proto_app_deployment]
      }

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:client_connected, ^hub_id}
      assert_receive {:app_deployment_started, ^app_deployment}, 5000
      assert app_deployment in TeamClient.get_app_deployments(team.id)

      # deletes the app deployment
      user_connected = %{user_connected | app_deployments: []}
      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:client_connected, ^hub_id}
      assert_receive {:app_deployment_stopped, ^app_deployment}
      refute app_deployment in TeamClient.get_app_deployments(team.id)
    end

    test "dispatches the agents list", %{team: team, pid: pid, user_connected: user_connected} do
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
    @describetag teams_for: :agent

    setup context do
      agent_connected =
        %LivebookProto.AgentConnected{
          name: get_in(context, [:agent, Access.key!(:name)]),
          public_key: context.org_key_pair.public_key,
          deployment_group_id: context.deployment_group.id,
          secrets: [],
          file_systems: [],
          deployment_groups: [],
          app_deployments: [],
          agents: []
        }

      pid = TeamClient.get_pid(context.team.id)
      {:ok, pid: pid, agent_connected: agent_connected}
    end

    test "dispatches the secrets list", %{team: team, pid: pid, agent_connected: agent_connected} do
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
      refute_received {:secret_created, ^secret}
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

    test "dispatches the file systems list",
         %{team: team, pid: pid, agent_connected: agent_connected} do
      bucket_url = "https://mybucket.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      fs_id = "#{team.id}-s3-#{Base.url_encode64(hash, padding: false)}"

      file_system =
        build(:fs_s3, id: fs_id, bucket_url: bucket_url, external_id: "123456", hub_id: team.id)

      type = Livebook.FileSystems.type(file_system)
      %{name: name} = Livebook.FileSystem.external_metadata(file_system)
      attrs = Livebook.FileSystem.dump(file_system)
      credentials = JSON.encode!(attrs)

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
      refute_received {:file_system_created, ^file_system}
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
      updated_credentials = JSON.encode!(updated_attrs)
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
         %{
           team: team,
           pid: pid,
           deployment_group: teams_deployment_group,
           agent_key: teams_agent_key,
           agent_connected: agent_connected
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
          agent_keys: [livebook_proto_agent_key],
          teams_auth: deployment_group.teams_auth,
          secrets: [],
          environment_variables: []
        }

      # creates the deployment group
      agent_connected = %{agent_connected | deployment_groups: [livebook_proto_deployment_group]}
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
         %{
           team: team,
           pid: pid,
           deployment_group: teams_deployment_group,
           agent_connected: agent_connected
         } do
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
      refute_received {:secret_created, ^secret}
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
          secrets: [livebook_proto_deployment_group_secret],
          teams_auth: deployment_group.teams_auth
        }

      agent_connected = %{agent_connected | deployment_groups: [livebook_proto_deployment_group]}
      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:deployment_group_created, ^deployment_group}
      refute secret in TeamClient.get_secrets(team.id)
      assert override_secret in TeamClient.get_secrets(team.id)
    end

    @tag :tmp_dir
    @tag teams_persisted: false
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
          agent_keys: [livebook_proto_agent_key],
          teams_auth: deployment_group.teams_auth
        }

      agent_connected = %{agent_connected | deployment_groups: [livebook_proto_deployment_group]}

      # creates a new app deployment
      deployment_group_id = to_string(deployment_group.id)
      slug = Livebook.Utils.random_short_id()
      title = "MyNotebook2-#{slug}"

      notebook = %{
        Livebook.Notebook.new()
        | app_settings: %{Livebook.Notebook.AppSettings.new() | slug: slug},
          file_entries: [%{type: :attachment, name: "image.jpg"}],
          name: title,
          hub_id: team.id,
          deployment_group_id: deployment_group_id
      }

      files_dir = Livebook.FileSystem.File.local(tmp_dir)
      image_file = Livebook.FileSystem.File.resolve(files_dir, "image.jpg")
      :ok = Livebook.FileSystem.File.write(image_file, "content")

      # since the app deployment must be exported to .livemd
      # it will call Teams to stamp the notebook, which
      # requires an user session
      id = team.id
      user = TeamsRPC.create_user(node)
      session_token = TeamsRPC.associate_user_with_org(node, user, teams_org)
      deployment_group_id = to_string(deployment_group.id)
      org_id = to_string(teams_org.id)
      team_user = %{team | user_id: user.id, session_token: session_token}
      Livebook.Hubs.save_hub(team_user)

      # check if it connected as User
      assert_receive {:hub_connected, ^id}
      assert_receive {:client_connected, ^id}

      refute_receive {:agent_joined,
                      %{hub_id: ^id, deployment_group_id: ^deployment_group_id, org_id: ^org_id}}

      # get the pid for user session, so we can guarantee the hub is deleted later
      pid = TeamClient.get_pid(id)

      {:ok, %Livebook.Teams.AppDeployment{file: zip_content} = app_deployment} =
        Livebook.Teams.AppDeployment.new(notebook, files_dir)

      # now we change to agent session
      TeamClient.stop(id)
      refute Process.alive?(pid)

      Livebook.Hubs.save_hub(team)
      pid = TeamClient.get_pid(id)

      # check if it connected again as Agent
      assert Process.alive?(pid)
      assert_receive {:hub_connected, ^id}, 3_000
      assert_receive {:client_connected, ^id}, 3_000

      assert_receive {:agent_joined,
                      %{hub_id: ^id, deployment_group_id: ^deployment_group_id, org_id: ^org_id} =
                        agent},
                     3_000

      secret_key = Livebook.Teams.derive_key(team.teams_key)
      encrypted_content = Livebook.Teams.encrypt(zip_content, secret_key)

      teams_app_deployment =
        TeamsRPC.upload_app_deployment(
          node,
          teams_org,
          teams_deployment_group,
          app_deployment,
          encrypted_content
        )

      # Since the app deployment struct generation is from Livebook side,
      # we don't have yet the information about who deployed the app,
      # so we need to add it ourselves.
      app_deployment = %{
        app_deployment
        | id: to_string(teams_app_deployment.id),
          version: Livebook.Utils.random_id(),
          file: nil,
          deployed_by: teams_app_deployment.app_revision.created_by.name,
          deployed_at: DateTime.from_naive!(teams_app_deployment.updated_at, "Etc/UTC")
      }

      {seconds, 0} = DateTime.to_gregorian_seconds(app_deployment.deployed_at)

      livebook_proto_app_deployment =
        %LivebookProto.AppDeployment{
          id: app_deployment.id,
          title: app_deployment.title,
          version: app_deployment.version,
          slug: app_deployment.slug,
          sha: app_deployment.sha,
          deployed_by: app_deployment.deployed_by,
          deployed_at: seconds,
          revision_id: to_string(teams_app_deployment.app_revision.id),
          deployment_group_id: app_deployment.deployment_group_id,
          multi_session: app_deployment.multi_session,
          access_type: to_string(app_deployment.access_type),
          authorization_groups: []
        }

      agent_connected = %{
        agent_connected
        | name: agent.name,
          app_deployments: [livebook_proto_app_deployment]
      }

      Livebook.Apps.subscribe()
      TeamsRPC.subscribe(node, self(), teams_deployment_group, teams_org)

      assert TeamsRPC.get_apps_metadatas(node, deployment_group_id) == %{}

      send(pid, {:event, :agent_connected, agent_connected})
      assert_receive {:app_deployment_started, ^app_deployment}

      [app_spec] = Livebook.Hubs.Provider.get_app_specs(team)
      Livebook.Apps.Manager.sync_permanent_apps()

      assert_receive {:app_created, %{slug: ^slug}}, 3_000
      assert_receive {:teams_broadcast, {:agent_updated, _agent}}

      assert TeamsRPC.get_apps_metadatas(node, deployment_group_id) == %{
               app_spec.version => %{
                 id: app_spec.app_deployment_id,
                 status: :preparing,
                 deployment_group_id: deployment_group_id
               }
             }

      assert_receive {:app_updated,
                      %{
                        slug: ^slug,
                        warnings: [],
                        sessions: [%{app_status: %{execution: :executed}}]
                      }}

      assert app_deployment in TeamClient.get_app_deployments(team.id)
      assert_receive {:teams_broadcast, {:agent_updated, _agent}}

      assert TeamsRPC.get_apps_metadatas(node, deployment_group_id) == %{
               app_spec.version => %{
                 id: app_spec.app_deployment_id,
                 status: :available,
                 deployment_group_id: deployment_group_id
               }
             }

      TeamsRPC.toggle_app_deployment(node, app_deployment.id, teams_org.id)

      assert_receive {:app_deployment_stopped, ^app_deployment}
      refute app_deployment in TeamClient.get_app_deployments(team.id)

      assert_receive {:app_closed,
                      %{
                        slug: ^slug,
                        warnings: [],
                        sessions: [%{app_status: %{execution: :executed, lifecycle: :active}}]
                      }}

      assert_receive {:teams_broadcast, {:agent_updated, _agent}}
      assert TeamsRPC.get_apps_metadatas(node, deployment_group_id) == %{}
    end

    test "dispatches the agents list",
         %{
           team: team,
           agent: agent,
           pid: pid,
           agent_connected: agent_connected,
           deployment_group: deployment_group
         } do
      send(pid, {:event, :agent_joined, agent})
      assert_receive {:agent_joined, ^agent}
      assert agent in TeamClient.get_agents(team.id)

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
end
