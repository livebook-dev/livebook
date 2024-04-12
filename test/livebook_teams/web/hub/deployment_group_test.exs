defmodule LivebookWeb.Integration.Hub.DeploymentGroupTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.Teams.DeploymentGroup

  setup %{user: user, node: node} do
    Livebook.Hubs.Broadcasts.subscribe([:crud, :connection, :secrets, :file_systems])
    Livebook.Teams.Broadcasts.subscribe([:clients, :app_deployments, :deployment_groups, :agents])
    hub = create_team_hub(user, node)
    id = hub.id

    assert_receive {:hub_connected, ^id}
    assert_receive {:client_connected, ^id}

    {:ok, hub: hub}
  end

  test "creates a deployment group", %{conn: conn, hub: hub} do
    deployment_group =
      build(:deployment_group,
        name: "TEAMS_ADD_DEPLOYMENT_GROUP",
        mode: :offline,
        hub_id: hub.id
      )

    attrs = %{
      deployment_group: %{
        name: deployment_group.name,
        value: deployment_group.mode,
        hub_id: deployment_group.hub_id
      }
    }

    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

    assert view
           |> element("#add-deployment-group")
           |> render_click(%{}) =~ "Add deployment group"

    view
    |> element("#deployment-group-form")
    |> render_change(attrs)

    refute view
           |> element("#deployment-group-form button[disabled]")
           |> has_element?()

    view
    |> element("#deployment-group-form")
    |> render_submit(attrs)

    assert_receive {:deployment_group_created,
                    %DeploymentGroup{name: "TEAMS_ADD_DEPLOYMENT_GROUP"} = deployment_group}

    assert_patch(view, "/hub/#{hub.id}")
    assert render(view) =~ "Deployment group added successfully"
    assert deployment_group in Livebook.Teams.get_deployment_groups(hub)

    # Guarantee it shows the error from API
    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

    assert view
           |> element("#add-deployment-group")
           |> render_click(%{}) =~ "Add deployment group"

    assert view
           |> element("#deployment-group-form")
           |> render_submit(attrs) =~ "has already been taken"
  end

  test "creates a secret", %{conn: conn, hub: hub} do
    %{id: id} =
      insert_deployment_group(
        name: "TEAMS_EDIT_DEPLOYMENT_GROUP",
        mode: :online,
        hub_id: hub.id
      )

    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

    secret =
      build(:secret,
        name: "DEPLOYMENT_GROUP_SECRET",
        hub_id: hub.id,
        deployment_group_id: id
      )

    attrs = %{
      secret: %{
        name: secret.name,
        value: secret.value,
        hub_id: secret.hub_id,
        deployment_group_id: secret.deployment_group_id
      }
    }

    refute render(view) =~ secret.name

    view
    |> element("#hub-deployment-group-#{id} [aria-label=\"add secret\"]", "Add secret")
    |> render_click(%{})

    assert_patch(view, ~p"/hub/#{hub.id}/groups/#{id}/secrets/new")
    assert render(view) =~ "Add secret"

    view
    |> element("#deployment-group-secrets-form")
    |> render_change(attrs)

    refute view
           |> element("#deployment-group-secrets-form button[disabled]")
           |> has_element?()

    view
    |> element("#deployment-group-secrets-form")
    |> render_submit(attrs)

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: [^secret]} =
                      deployment_group}

    assert_patch(view, ~p"/hub/#{hub.id}")
    assert render(view) =~ "Secret DEPLOYMENT_GROUP_SECRET added successfully"
    assert render(element(view, "#hub-deployment-group-#{id}")) =~ secret.name
    assert secret in deployment_group.secrets

    # Guarantee it shows the error from API
    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}/groups/#{id}/secrets/new")

    assert view
           |> element("#deployment-group-secrets-form")
           |> render_submit(attrs) =~ "has already been taken"
  end

  test "updates an existing secret", %{conn: conn, hub: hub} do
    %{id: id} =
      insert_deployment_group(name: "TEAMS_EDIT_DEPLOYMENT_GROUP", mode: :online, hub_id: hub.id)

    secret =
      insert_secret(name: "DEPLOYMENT_GROUP_EDIT_SECRET", hub_id: hub.id, deployment_group_id: id)

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: [^secret]}}

    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

    assert view
           |> element("#hub-deployment-group-#{id} [aria-label=\"edit #{secret.name}\"]")
           |> render_click() =~ "Edit secret"

    assert_patch(view, ~p"/hub/#{hub.id}/groups/#{id}/secrets/edit/#{secret.name}")

    attrs = %{
      secret: %{
        name: secret.name,
        value: secret.value,
        hub_id: secret.hub_id
      }
    }

    new_value = "new_value"

    view
    |> element("#deployment-group-secrets-form")
    |> render_change(attrs)

    refute view
           |> element("#deployment-group-secrets-form button[disabled]")
           |> has_element?()

    view
    |> element("#deployment-group-secrets-form")
    |> render_submit(put_in(attrs.secret.value, new_value))

    updated_secret = %{secret | value: new_value}

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: [^updated_secret]} =
                      deployment_group}

    assert_patch(view, "/hub/#{hub.id}")
    assert render(view) =~ "Secret DEPLOYMENT_GROUP_EDIT_SECRET updated successfully"
    assert render(element(view, "#hub-deployment-group-#{id}")) =~ secret.name
    assert updated_secret in deployment_group.secrets
  end

  test "deletes an existing secret", %{conn: conn, hub: hub} do
    %{id: id} =
      insert_deployment_group(name: "TEAMS_EDIT_DEPLOYMENT_GROUP", mode: :online, hub_id: hub.id)

    secret =
      insert_secret(
        name: "DEPLOYMENT_GROUP_DELETE_SECRET",
        hub_id: hub.id,
        deployment_group_id: id
      )

    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

    view
    |> element("#hub-deployment-group-#{id} [aria-label=\"delete #{secret.name}\"]")
    |> render_click()

    render_confirm(view)

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: []}}

    assert_patch(view, ~p"/hub/#{hub.id}")
    assert render(view) =~ "Secret DEPLOYMENT_GROUP_DELETE_SECRET deleted successfully"
    refute render(element(view, "#hub-deployment-group-#{id}")) =~ secret.name
  end

  test "shows the agent count", %{conn: conn, hub: hub} do
    name = "TEAMS_EDIT_DEPLOYMENT_GROUP2"
    %{id: id} = insert_deployment_group(name: name, mode: :online, hub_id: hub.id)

    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

    assert view
           |> element("#hub-deployment-group-#{id} [aria-label=\"app servers\"]")
           |> render()
           |> Floki.parse_fragment!()
           |> Floki.text()
           |> String.trim() == "0"

    org_id = to_string(hub.org_id)

    # Simulates the agent join event
    pid = Livebook.Hubs.TeamClient.get_pid(hub.id)
    agent = build(:agent, hub_id: hub.id, org_id: org_id, deployment_group_id: to_string(id))

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

    assert view
           |> element("#hub-deployment-group-#{id} [aria-label=\"app servers\"]")
           |> render()
           |> Floki.parse_fragment!()
           |> Floki.text()
           |> String.trim() == "1"
  end

  @tag :tmp_dir
  test "shows the app deployed count", %{conn: conn, hub: hub, tmp_dir: tmp_dir} do
    name = "TEAMS_EDIT_DEPLOYMENT_GROUP3"
    %{id: id} = insert_deployment_group(name: name, mode: :online, hub_id: hub.id)

    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

    assert view
           |> element("#hub-deployment-group-#{id} [aria-label=\"apps deployed\"]")
           |> render()
           |> Floki.parse_fragment!()
           |> Floki.text()
           |> String.trim() == "0"

    app_settings = %{Livebook.Notebook.AppSettings.new() | slug: Livebook.Utils.random_short_id()}

    notebook = %{
      Livebook.Notebook.new()
      | app_settings: app_settings,
        name: "MyNotebook",
        hub_id: hub.id,
        deployment_group_id: to_string(id)
    }

    files_dir = Livebook.FileSystem.File.local(tmp_dir)

    {:ok, app_deployment} = Livebook.Teams.AppDeployment.new(notebook, files_dir)
    :ok = Livebook.Teams.deploy_app(hub, app_deployment, app_settings)

    assert_receive {:app_deployment_started, _}

    assert view
           |> element("#hub-deployment-group-#{id} [aria-label=\"apps deployed\"]")
           |> render()
           |> Floki.parse_fragment!()
           |> Floki.text()
           |> String.trim() == "1"
  end
end
