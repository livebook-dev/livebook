defmodule Livebook.TeamsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.{FileSystem, Notebook, Teams, Utils}
  alias Livebook.Teams.Org

  setup do
    Livebook.Hubs.Broadcasts.subscribe([:connection, :file_systems, :secrets])
    Livebook.Teams.Broadcasts.subscribe([:clients, :deployment_groups, :app_deployments, :agents])

    :ok
  end

  describe "create_org/1" do
    test "returns the device flow data to confirm the org creation" do
      org = build(:org)

      assert {:ok,
              %{
                "device_code" => _device_code,
                "expires_in" => 300,
                "id" => _org_id,
                "user_code" => _user_code,
                "verification_uri" => _verification_uri
              }} = Teams.create_org(org, %{})
    end

    test "returns changeset errors when data is invalid" do
      org = build(:org)

      assert {:error, changeset} = Teams.create_org(org, %{name: nil})
      assert "can't be blank" in errors_on(changeset).name
    end
  end

  describe "join_org/1" do
    test "returns the device flow data to confirm the org creation", %{user: user, node: node} do
      org = build(:org)
      key_hash = Org.key_hash(org)

      teams_org = :erpc.call(node, TeamsRPC, :create_org, [[name: org.name]])

      :erpc.call(node, TeamsRPC, :create_org_key, [
        [org: teams_org, key_hash: key_hash]
      ])

      :erpc.call(node, TeamsRPC, :create_user_org, [[org: teams_org, user: user]])

      assert {:ok,
              %{
                "device_code" => _device_code,
                "expires_in" => 300,
                "id" => _org_id,
                "user_code" => _user_code,
                "verification_uri" => _verification_uri
              }} = Teams.join_org(org, %{})
    end

    test "returns changeset errors when data is invalid" do
      org = build(:org)

      assert {:error, changeset} = Teams.join_org(org, %{name: nil})
      assert "can't be blank" in errors_on(changeset).name
    end

    test "returns changeset errors when org doesn't exist" do
      org = build(:org)

      assert {:error, changeset} = Teams.join_org(org, %{})
      assert "does not exist" in errors_on(changeset).name
      assert "does not match existing key" in errors_on(changeset).teams_key
    end
  end

  describe "get_org_request_completion_data/1" do
    test "returns the org data when it has been confirmed", %{node: node, user: user} do
      teams_key = Org.teams_key()
      key_hash = :crypto.hash(:sha256, teams_key) |> Base.url_encode64(padding: false)

      org_request =
        :erpc.call(node, TeamsRPC, :create_org_request, [[key_hash: key_hash]])

      org_request =
        :erpc.call(node, TeamsRPC, :confirm_org_request, [org_request, user])

      org =
        build(:org,
          id: org_request.id,
          name: org_request.name,
          teams_key: teams_key,
          user_code: org_request.user_code
        )

      %{
        token: token,
        user_org: %{
          org: %{
            id: id,
            name: name,
            keys: [%{id: org_key_id}],
            key_pair: %{public_key: org_public_key}
          },
          user: %{id: user_id}
        }
      } = org_request.user_org_session

      assert Teams.get_org_request_completion_data(org, org_request.device_code) ==
               {:ok,
                %{
                  "id" => id,
                  "name" => name,
                  "org_key_id" => org_key_id,
                  "org_public_key" => org_public_key,
                  "session_token" => token,
                  "user_id" => user_id
                }}
    end

    test "returns the org request awaiting confirmation", %{node: node} do
      teams_key = Org.teams_key()
      key_hash = :crypto.hash(:sha256, teams_key) |> Base.url_encode64(padding: false)

      org_request =
        :erpc.call(node, TeamsRPC, :create_org_request, [[key_hash: key_hash]])

      org =
        build(:org,
          id: org_request.id,
          name: org_request.name,
          teams_key: teams_key,
          user_code: org_request.user_code
        )

      assert Teams.get_org_request_completion_data(org, org_request.device_code) ==
               {:ok, :awaiting_confirmation}
    end

    test "returns error when org request doesn't exist" do
      org = build(:org, id: 0)

      assert {:transport_error, _embarrassing} =
               Teams.get_org_request_completion_data(org, "")
    end

    test "returns error when org request expired", %{node: node} do
      now = NaiveDateTime.truncate(NaiveDateTime.utc_now(), :second)
      expires_at = NaiveDateTime.add(now, -5000)
      teams_key = Org.teams_key()
      key_hash = :crypto.hash(:sha256, teams_key) |> Base.url_encode64(padding: false)

      org_request =
        :erpc.call(node, TeamsRPC, :create_org_request, [
          [expires_at: expires_at, key_hash: key_hash]
        ])

      org =
        build(:org,
          id: org_request.id,
          name: org_request.name,
          teams_key: teams_key,
          user_code: org_request.user_code
        )

      assert Teams.get_org_request_completion_data(org, org_request.device_code) ==
               {:error, :expired}
    end
  end

  describe "create_deployment_group/2" do
    test "creates a new deployment group when the data is valid", %{user: user, node: node} do
      team = connect_to_teams(user, node)
      attrs = params_for(:deployment_group, name: "DEPLOYMENT_GROUP_#{team.id}", mode: :online)

      assert {:ok, deployment_group} = Teams.create_deployment_group(team, attrs)

      %{id: id, name: name, mode: mode} = deployment_group

      assert_receive {:deployment_group_created, %{id: ^id, name: ^name, mode: ^mode}}

      # Guarantee uniqueness
      assert {:error, changeset} = Teams.create_deployment_group(team, attrs)
      assert "has already been taken" in errors_on(changeset).name
    end

    test "creates a new deployment group with Livebook Teams authentication",
         %{user: user, node: node} do
      team = connect_to_teams(user, node)
      attrs = params_for(:deployment_group, name: "DEPLOYMENT_GROUP_#{team.id}", mode: :online)

      assert {:ok, %{id: id, name: name, mode: mode, teams_auth: true}} =
               Teams.create_deployment_group(team, attrs)

      assert_receive {:deployment_group_created,
                      %{id: ^id, name: ^name, mode: ^mode, teams_auth: true}}
    end

    test "returns changeset errors when the name is invalid", %{user: user, node: node} do
      team = connect_to_teams(user, node)
      attrs = params_for(:deployment_group, name: "")

      assert {:error, changeset} = Teams.create_deployment_group(team, attrs)
      assert "can't be blank" in errors_on(changeset).name
    end

    test "returns changeset errors when the mode is invalid", %{user: user, node: node} do
      team = connect_to_teams(user, node)
      attrs = params_for(:deployment_group, mode: "invalid")

      assert {:error, changeset} = Teams.create_deployment_group(team, attrs)
      assert "is invalid" in errors_on(changeset).mode
    end
  end

  describe "deploy_app/2" do
    @tag :tmp_dir
    test "deploys app to Teams from a notebook", %{user: user, node: node, tmp_dir: tmp_dir} do
      team = connect_to_teams(user, node)
      attrs = params_for(:deployment_group, name: "BAZ", mode: :online)
      {:ok, %{id: id}} = Teams.create_deployment_group(team, attrs)

      assert_receive {:deployment_group_created, %{id: ^id}}

      # creates the app deployment
      slug = Utils.random_short_id()
      title = "MyNotebook-#{slug}"
      app_settings = %{Notebook.AppSettings.new() | slug: slug}

      notebook = %{
        Notebook.new()
        | app_settings: app_settings,
          name: title,
          hub_id: team.id,
          deployment_group_id: id
      }

      files_dir = FileSystem.File.local(tmp_dir)
      assert {:ok, app_deployment} = Teams.AppDeployment.new(notebook, files_dir)
      assert Teams.deploy_app(team, app_deployment) == :ok

      sha = app_deployment.sha
      multi_session = app_settings.multi_session
      access_type = app_settings.access_type

      assert_receive {:app_deployment_started,
                      %Livebook.Teams.AppDeployment{
                        slug: ^slug,
                        sha: ^sha,
                        title: ^title,
                        multi_session: ^multi_session,
                        access_type: ^access_type,
                        deployment_group_id: ^id
                      } = app_deployment2}

      assert {:error,
              %{errors: [slug: {"should only contain alphanumeric characters and dashes", []}]}} =
               Teams.deploy_app(team, %{app_deployment | slug: "@abc"})

      assert {:error, %{errors: [multi_session: {"can't be blank", []}]}} =
               Teams.deploy_app(team, %{app_deployment | multi_session: nil})

      assert {:error, %{errors: [access_type: {"can't be blank", []}]}} =
               Teams.deploy_app(team, %{app_deployment | access_type: nil})

      assert {:error, %{errors: [access_type: {"is invalid", []}]}} =
               Teams.deploy_app(team, %{app_deployment | access_type: :abc})

      # force app deployment to be stopped
      erpc_call(node, :toggle_app_deployment, [app_deployment2.id, team.org_id])
      assert_receive {:app_deployment_stopped, ^app_deployment2}
    end
  end
end
