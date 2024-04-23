defmodule Livebook.Teams.ConnectionTest do
  alias Livebook.FileSystem
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Teams.Connection

  describe "connect" do
    test "successfully authenticates the websocket connection", %{user: user, node: node} do
      {_, headers} = build_team_headers(user, node)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected
    end

    test "rejects the websocket connection with invalid credentials", %{user: user} do
      headers = [
        {"x-user", to_string(user.id)},
        {"x-org", to_string(user.id)},
        {"x-org-key", to_string(user.id)},
        {"x-session-token", "foo"}
      ]

      assert {:ok, _conn} = Connection.start_link(self(), headers)

      assert_receive {:server_error,
                      "Your session is out-of-date. Please re-join the organization."}

      assert {:ok, _conn} = Connection.start_link(self(), [])

      assert_receive {:server_error,
                      "Invalid request. Please re-join the organization and update Livebook if the issue persists."}
    end
  end

  describe "handle events" do
    test "receives the secret_created event", %{user: user, node: node} do
      {hub, headers} = build_team_headers(user, node)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected

      # creates a new secret
      secret = build(:secret, name: "FOO", value: "BAR")
      assert Livebook.Hubs.create_secret(hub, secret) == :ok

      # receives `{:event, :secret_created, secret_created}` event
      # without decrypting the value
      assert_receive {:event, :secret_created, secret_created}
      assert secret_created.name == secret.name
      refute secret_created.value == secret.value
    end

    test "receives the file_system_created event", %{user: user, node: node} do
      {hub, headers} = build_team_headers(user, node)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected

      # creates a new file system
      file_system = build(:fs_s3, bucket_url: "https://file_system_created.s3.amazonaws.com")
      assert Livebook.Hubs.create_file_system(hub, file_system) == :ok
      type = Livebook.FileSystems.type(file_system)
      %{name: name} = FileSystem.external_metadata(file_system)

      # receives `{:event, :file_system_created, file_system_created}` event
      # without decrypting the value
      assert_receive {:event, :file_system_created, file_system_created}
      assert file_system_created.name == name
      assert file_system_created.type == to_string(type)
      refute file_system_created.value == FileSystem.dump(file_system)
      assert is_binary(file_system_created.value)
    end

    test "receives the deployment_group_created event", %{user: user, node: node} do
      {hub, headers} = build_team_headers(user, node)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected

      # creates a new deployment group with offline mode
      deployment_group = build(:deployment_group, name: "FOO", mode: :offline, clustering: :dns)

      assert {:ok, _id} =
               Livebook.Teams.create_deployment_group(hub, deployment_group)

      # deployment_group name and mode are not encrypted
      assert_receive {:event, :deployment_group_created, deployment_group_created}
      assert deployment_group_created.name == deployment_group.name
      assert String.to_existing_atom(deployment_group_created.mode) == deployment_group.mode

      assert String.to_existing_atom(deployment_group_created.clustering) ==
               deployment_group.clustering

      # since the deployment group is with offline mode, the agent key shouldn't exists
      assert deployment_group_created.agent_keys == []

      # creates a new deployment group with online mode
      deployment_group = build(:deployment_group, name: "BAR", mode: :online, clustering: :dns)
      {:ok, _id} = Livebook.Teams.create_deployment_group(hub, deployment_group)

      # deployment_group name and mode are not encrypted
      assert_receive {:event, :deployment_group_created, deployment_group_created}
      assert deployment_group_created.name == deployment_group.name
      assert String.to_existing_atom(deployment_group_created.mode) == deployment_group.mode

      assert String.to_existing_atom(deployment_group_created.clustering) ==
               deployment_group.clustering

      # receives the built-in agent key
      assert [agent_key] = deployment_group_created.agent_keys
      assert is_binary(agent_key.key)
      assert agent_key.deployment_group_id == deployment_group_created.id
    end

    @tag :tmp_dir
    test "receives the app deployments list from user_connected event",
         %{user: user, node: node, tmp_dir: tmp_dir} do
      {hub, headers} = build_team_headers(user, node)

      # creates a new deployment group
      deployment_group = build(:deployment_group, name: "BAZ", mode: :online)
      {:ok, id} = Livebook.Teams.create_deployment_group(hub, deployment_group)

      # creates a new app deployment
      deployment_group_id = to_string(id)
      slug = Livebook.Utils.random_short_id()
      title = "MyNotebook3-#{slug}"
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}

      notebook = %{
        Livebook.Notebook.new()
        | app_settings: app_settings,
          name: title,
          hub_id: hub.id,
          deployment_group_id: deployment_group_id
      }

      files_dir = Livebook.FileSystem.File.local(tmp_dir)

      {:ok, %Livebook.Teams.AppDeployment{} = app_deployment} =
        Livebook.Teams.AppDeployment.new(notebook, files_dir)

      # since we want to fetch the app deployment from connection event,
      # we need to persist it before we connect to the WebSocket
      :ok = Livebook.Teams.deploy_app(hub, app_deployment)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected

      assert_receive {:event, :user_connected, user_connected}
      assert [app_deployment2] = user_connected.app_deployments
      assert app_deployment2.title == title
      assert app_deployment2.slug == slug
      assert app_deployment2.sha == app_deployment.sha
      assert app_deployment2.deployment_group_id == deployment_group_id
    end

    @tag :tmp_dir
    test "receives the app deployments list from agent_connected event",
         %{user: user, node: node, tmp_dir: tmp_dir} do
      # To create a new app deployment, we need use the User connection
      {hub, _headers} = build_team_headers(user, node)

      # creates a new deployment group
      deployment_group = build(:deployment_group, name: "BAZ", mode: :online)
      {:ok, id} = Livebook.Teams.create_deployment_group(hub, deployment_group)
      teams_deployment_group = erpc_call(node, :get_deployment_group!, [id])
      [teams_agent_key] = teams_deployment_group.agent_keys

      # creates a new app deployment
      slug = Livebook.Utils.random_short_id()
      title = "MyNotebook3-#{slug}"
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}

      notebook = %{
        Livebook.Notebook.new()
        | app_settings: app_settings,
          name: title,
          hub_id: hub.id,
          deployment_group_id: to_string(id)
      }

      files_dir = Livebook.FileSystem.File.local(tmp_dir)

      {:ok, %Livebook.Teams.AppDeployment{} = app_deployment} =
        Livebook.Teams.AppDeployment.new(notebook, files_dir)

      # since we want to fetch the app deployment from connection event,
      # we need to persist it before we connect to the WebSocket
      :ok = Livebook.Teams.deploy_app(hub, app_deployment)

      # As we need to be Agent to receive the app deployments list to be deployed,
      # we will create another connection here
      public_key = hub.org_public_key

      hub = %{
        hub
        | user_id: nil,
          org_public_key: nil,
          session_token: teams_agent_key.key
      }

      agent_name = Livebook.Config.agent_name()

      headers = [
        {"x-lb-version", Livebook.Config.app_version()},
        {"x-org", to_string(hub.org_id)},
        {"x-org-key", to_string(hub.org_key_id)},
        {"x-agent-name", agent_name},
        {"x-agent-key", hub.session_token}
      ]

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected

      assert_receive {:event, :agent_connected, agent_connected}
      assert agent_connected.name == agent_name
      assert agent_connected.public_key == public_key
      assert [app_deployment2] = agent_connected.app_deployments
      assert app_deployment2.title == title
      assert app_deployment2.slug == slug
      assert app_deployment2.sha == app_deployment.sha
      assert app_deployment2.deployment_group_id == to_string(id)
    end
  end
end
