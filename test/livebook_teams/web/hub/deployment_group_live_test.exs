defmodule LivebookWeb.Integration.Hub.DeploymentGroupLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.Teams.DeploymentGroup

  setup %{user: user, node: node} do
    Livebook.Hubs.Broadcasts.subscribe([:crud, :connection, :secrets, :file_systems])
    Livebook.Teams.Broadcasts.subscribe([:deployment_groups])
    hub = create_team_hub(user, node)
    id = hub.id

    assert_receive {:hub_connected, ^id}

    {:ok, hub: hub}
  end

  test "creates a deployment group", %{conn: conn, hub: hub} do
    deployment_group =
      build(:deployment_group,
        name: "TEAMS_ADD_DEPLOYMENT_GROUP",
        mode: "offline",
        hub_id: hub.id
      )

    attrs = %{
      deployment_group: %{
        name: deployment_group.name,
        value: deployment_group.mode,
        hub_id: deployment_group.hub_id
      }
    }

    {:ok, view, html} = live(conn, ~p"/hub/#{hub.id}/deployment-groups/new")
    assert html =~ "Add a new deployment group to"

    view
    |> element("#deployment-groups-form")
    |> render_change(attrs)

    refute view
           |> element("#deployment-groups-form button[disabled]")
           |> has_element?()

    view
    |> element("#deployment-groups-form")
    |> render_submit(attrs)

    assert_receive {:deployment_group_created,
                    %DeploymentGroup{id: id, name: "TEAMS_ADD_DEPLOYMENT_GROUP"} =
                      deployment_group}

    assert_patch(view, "/hub/#{hub.id}/deployment-groups/edit/#{id}")
    assert render(view) =~ "Deployment group TEAMS_ADD_DEPLOYMENT_GROUP added successfully"
    assert deployment_group in Livebook.Teams.get_deployment_groups(hub)

    # Guarantee it shows the error from API

    {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}/deployment-groups/new")

    view
    |> element("#deployment-groups-form")
    |> render_submit(attrs)

    assert render(view) =~ "has already been taken"
  end

  test "updates an existing deployment group", %{conn: conn, hub: hub} do
    insert_deployment_group(
      name: "TEAMS_EDIT_DEPLOYMENT_GROUP",
      mode: "online",
      hub_id: hub.id
    )

    assert_receive {:deployment_group_created,
                    %DeploymentGroup{name: "TEAMS_EDIT_DEPLOYMENT_GROUP"} = deployment_group}

    attrs = %{
      deployment_group: %{
        id: deployment_group.id,
        name: deployment_group.name,
        mode: deployment_group.mode,
        hub_id: deployment_group.hub_id
      }
    }

    new_mode = "offline"

    {:ok, view, html} =
      live(conn, ~p"/hub/#{hub.id}/deployment-groups/edit/#{deployment_group.id}")

    assert html =~ "Edit deployment group"
    assert html =~ "Manage the #{deployment_group.name} deployment group"

    view
    |> element("#deployment-groups-form")
    |> render_change(attrs)

    refute view
           |> element("#deployment-groups-form button[disabled]")
           |> has_element?()

    view
    |> element("#deployment-groups-form")
    |> render_submit(put_in(attrs.deployment_group.mode, new_mode))

    updated_deployment_group = %{deployment_group | mode: new_mode}

    assert_receive {:deployment_group_updated, ^updated_deployment_group}
    assert_patch(view, "/hub/#{hub.id}/deployment-groups/edit/#{deployment_group.id}")
    assert render(view) =~ "Deployment group TEAMS_EDIT_DEPLOYMENT_GROUP updated successfully"
    assert updated_deployment_group in Livebook.Teams.get_deployment_groups(hub)
  end

  test "creates a secret", %{conn: conn, hub: hub} do
    insert_deployment_group(
      name: "TEAMS_EDIT_DEPLOYMENT_GROUP",
      mode: "online",
      hub_id: hub.id
    )

    hub_id = hub.id

    assert_receive {:deployment_group_created,
                    %DeploymentGroup{name: "TEAMS_EDIT_DEPLOYMENT_GROUP", hub_id: ^hub_id} =
                      deployment_group}

    id = deployment_group.id

    {:ok, view, _html} =
      live(conn, ~p"/hub/#{hub.id}/deployment-groups/edit/#{deployment_group.id}")

    secret =
      build(:secret,
        name: "DEPLOYMENT_GROUP_SECRET",
        hub_id: hub_id,
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
    |> element("#add-secret")
    |> render_click(%{})

    assert_patch(
      view,
      ~p"/hub/#{hub_id}/deployment-groups/edit/#{id}/secrets/new"
    )

    assert render(view) =~ "Add secret"

    view
    |> element("#secrets-form")
    |> render_change(attrs)

    refute view
           |> element("#secrets-form button[disabled]")
           |> has_element?()

    view
    |> element("#secrets-form")
    |> render_submit(attrs)

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: [^secret]} =
                      deployment_group}

    assert_patch(view, "/hub/#{hub_id}/deployment-groups/edit/#{id}")
    assert render(view) =~ "Secret DEPLOYMENT_GROUP_SECRET added successfully"
    assert render(element(view, "#deployment-group-secrets-list")) =~ secret.name
    assert secret in deployment_group.secrets

    # Guarantee it shows the error from API

    {:ok, view, _html} =
      live(conn, ~p"/hub/#{hub_id}/deployment-groups/edit/#{id}/secrets/new")

    view
    |> element("#secrets-form")
    |> render_submit(attrs)

    assert render(view) =~ "has already been taken"
  end

  test "updates an existing secret", %{conn: conn, hub: hub} do
    insert_deployment_group(
      name: "TEAMS_EDIT_DEPLOYMENT_GROUP",
      mode: "online",
      hub_id: hub.id
    )

    hub_id = hub.id

    assert_receive {:deployment_group_created,
                    %DeploymentGroup{name: "TEAMS_EDIT_DEPLOYMENT_GROUP", hub_id: ^hub_id} =
                      deployment_group}

    id = deployment_group.id

    secret =
      insert_secret(
        name: "DEPLOYMENT_GROUP_EDIT_SECRET",
        hub_id: hub_id,
        deployment_group_id: id
      )

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: [^secret]}}

    {:ok, view, _html} =
      live(conn, ~p"/hub/#{hub_id}/deployment-groups/edit/#{id}")

    attrs = %{
      secret: %{
        name: secret.name,
        value: secret.value,
        hub_id: secret.hub_id
      }
    }

    new_value = "new_value"

    view
    |> element("#hub-secret-#{secret.name}-edit")
    |> render_click(%{"secret_name" => secret.name})

    assert_patch(
      view,
      ~p"/hub/#{hub_id}/deployment-groups/edit/#{id}/secrets/edit/#{secret.name}"
    )

    assert render(view) =~ "Edit secret"

    view
    |> element("#secrets-form")
    |> render_change(attrs)

    refute view
           |> element("#secrets-form button[disabled]")
           |> has_element?()

    view
    |> element("#secrets-form")
    |> render_submit(put_in(attrs.secret.value, new_value))

    updated_secret = %{secret | value: new_value}

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: [^updated_secret]} =
                      deployment_group}

    assert_patch(view, "/hub/#{hub_id}/deployment-groups/edit/#{id}")
    assert render(view) =~ "Secret DEPLOYMENT_GROUP_EDIT_SECRET updated successfully"
    assert render(element(view, "#deployment-group-secrets-list")) =~ secret.name
    assert updated_secret in deployment_group.secrets
  end

  test "deletes an existing secret", %{conn: conn, hub: hub} do
    insert_deployment_group(
      name: "TEAMS_EDIT_DEPLOYMENT_GROUP",
      mode: "online",
      hub_id: hub.id
    )

    hub_id = hub.id

    assert_receive {:deployment_group_created,
                    %DeploymentGroup{name: "TEAMS_EDIT_DEPLOYMENT_GROUP", hub_id: ^hub_id} =
                      deployment_group}

    id = deployment_group.id

    secret =
      insert_secret(
        name: "DEPLOYMENT_GROUP_DELETE_SECRET",
        hub_id: hub_id,
        deployment_group_id: id
      )

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: [^secret]}}

    {:ok, view, _html} =
      live(conn, ~p"/hub/#{hub_id}/deployment-groups/edit/#{id}")

    refute view
           |> element("#secrets-form button[disabled]")
           |> has_element?()

    view
    |> element("#hub-secret-#{secret.name}-delete")
    |> render_click()

    render_confirm(view)

    assert_receive {:deployment_group_updated,
                    %Livebook.Teams.DeploymentGroup{id: ^id, secrets: []} = deployment_group}

    assert_patch(view, "/hub/#{hub_id}/deployment-groups/edit/#{id}")
    assert render(view) =~ "Secret DEPLOYMENT_GROUP_DELETE_SECRET deleted successfully"
    refute render(element(view, "#deployment-group-secrets-list")) =~ secret.name
    refute secret in deployment_group.secrets
  end
end
