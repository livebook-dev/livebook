defmodule LivebookWeb.Integration.Hub.EditLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.Hubs
  alias Livebook.Teams.DeploymentGroup

  describe "user" do
    setup %{user: user, node: node} do
      Livebook.Hubs.Broadcasts.subscribe([:crud, :connection, :secrets, :file_systems])
      Livebook.Teams.Broadcasts.subscribe([:deployment_groups])
      hub = create_team_hub(user, node)
      id = hub.id

      assert_receive {:hub_connected, ^id}

      {:ok, hub: hub}
    end

    test "updates the hub", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{"hub_emoji" => "🐈"}

      view
      |> element("#team-form")
      |> render_change(%{"team" => attrs})

      refute view
             |> element("#team-form .invalid-feedback")
             |> has_element?()

      view
      |> element("#team-form")
      |> render_submit(%{"team" => attrs})

      update = render(view)
      assert update =~ "Hub updated successfully"
      assert update =~ "🐈"

      id = hub.id
      assert_receive {:hub_changed, ^id}

      assert_sidebar_hub(view, id, hub.hub_name, attrs["hub_emoji"])
      refute Hubs.fetch_hub!(hub.id) == hub
    end

    test "deletes the hub", %{conn: conn, hub: hub} do
      id = hub.id
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      view
      |> element("#delete-hub", "Delete hub")
      |> render_click()

      render_confirm(view)

      assert_receive {:hub_changed, ^id}
      %{"success" => "Hub deleted successfully"} = assert_redirect(view, "/")

      {:ok, view, _html} = live(conn, ~p"/")

      refute_sidebar_hub(view, id)
      assert_raise Livebook.Storage.NotFoundError, fn -> Hubs.fetch_hub!(id) end
    end

    test "creates a secret", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      secret = build(:secret, name: "TEAM_ADD_SECRET", hub_id: hub.id)

      attrs = %{
        secret: %{
          name: secret.name,
          value: secret.value,
          hub_id: secret.hub_id
        }
      }

      refute render(view) =~ secret.name

      view
      |> element("#add-secret")
      |> render_click(%{})

      assert_patch(view, ~p"/hub/#{hub.id}/secrets/new")
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

      assert_receive {:secret_created, ^secret}
      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "Secret TEAM_ADD_SECRET added successfully"
      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert secret in Livebook.Hubs.get_secrets(hub)

      # Guarantee it shows the error from API

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}/secrets/new")

      view
      |> element("#secrets-form")
      |> render_submit(attrs)

      assert render(view) =~ "has already been taken"
    end

    test "updates existing secret", %{conn: conn, hub: hub} do
      secret = insert_secret(name: "TEAM_EDIT_SECRET", hub_id: hub.id)
      assert_receive {:secret_created, ^secret}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

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

      assert_patch(view, ~p"/hub/#{hub.id}/secrets/edit/#{secret.name}")
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

      assert_receive {:secret_updated, ^updated_secret}
      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "Secret TEAM_EDIT_SECRET updated successfully"
      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert updated_secret in Livebook.Hubs.get_secrets(hub)
    end

    test "deletes existing secret", %{conn: conn, hub: hub} do
      secret = insert_secret(name: "TEAM_DELETE_SECRET", hub_id: hub.id)
      assert_receive {:secret_created, ^secret}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      refute view
             |> element("#secrets-form button[disabled]")
             |> has_element?()

      view
      |> element("#hub-secret-#{secret.name}-delete")
      |> render_click()

      render_confirm(view)

      assert_receive {:secret_deleted, ^secret}
      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "Secret TEAM_DELETE_SECRET deleted successfully"
      refute render(element(view, "#hub-secrets-list")) =~ secret.name
      refute secret in Livebook.Hubs.get_secrets(hub)
    end

    test "raises an error if does not exist secret", %{conn: conn, hub: hub} do
      assert_raise LivebookWeb.NotFoundError, fn ->
        live(conn, ~p"/hub/#{hub.id}/secrets/edit/HELLO")
      end
    end

    test "creates a file system", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      bypass = Bypass.open()
      file_system = build_bypass_file_system(bypass, hub.id)
      id = file_system.id
      attrs = %{file_system: Livebook.FileSystem.dump(file_system)}

      expect_s3_listing(bypass)

      refute render(view) =~ file_system.bucket_url

      view
      |> element("#add-file-system")
      |> render_click(%{})

      assert_patch(view, ~p"/hub/#{hub.id}/file-systems/new")
      assert render(view) =~ "Add file storage"

      view
      |> element("#file-systems-form")
      |> render_change(attrs)

      refute view
             |> element("#file-systems-form button[disabled]")
             |> has_element?()

      view
      |> element("#file-systems-form")
      |> render_submit(attrs)

      assert_receive {:file_system_created, %{id: ^id} = file_system}
      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "File storage added successfully"
      assert render(element(view, "#hub-file-systems-list")) =~ file_system.bucket_url
      assert file_system in Livebook.Hubs.get_file_systems(hub)
    end

    test "updates existing file system", %{conn: conn, hub: hub} do
      bypass = Bypass.open()
      file_system = build_bypass_file_system(bypass, hub.id)
      id = file_system.id

      :ok = Hubs.create_file_system(hub, file_system)
      assert_receive {:file_system_created, %{id: ^id} = file_system}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{file_system: Livebook.FileSystem.dump(file_system)}

      expect_s3_listing(bypass)

      view
      |> element("#hub-file-system-#{file_system.id}-edit")
      |> render_click(%{"file_system" => file_system})

      assert_patch(view, ~p"/hub/#{hub.id}/file-systems/edit/#{file_system.id}")
      assert render(view) =~ "Edit file storage"

      view
      |> element("#file-systems-form")
      |> render_change(attrs)

      refute view
             |> element("#file-systems-form button[disabled]")
             |> has_element?()

      view
      |> element("#file-systems-form")
      |> render_submit(put_in(attrs.file_system.access_key_id, "new key"))

      updated_file_system = %{file_system | access_key_id: "new key"}

      assert_receive {:file_system_updated, ^updated_file_system}
      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "File storage updated successfully"
      assert render(element(view, "#hub-file-systems-list")) =~ file_system.bucket_url
      assert updated_file_system in Livebook.Hubs.get_file_systems(hub)
    end

    test "detaches existing file system", %{conn: conn, hub: hub} do
      bypass = Bypass.open()
      file_system = build_bypass_file_system(bypass, hub.id)
      id = file_system.id

      :ok = Hubs.create_file_system(hub, file_system)
      assert_receive {:file_system_created, %{id: ^id} = file_system}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      refute view
             |> element("#file-systems-form button[disabled]")
             |> has_element?()

      view
      |> element("#hub-file-system-#{file_system.id}-detach", "Detach")
      |> render_click()

      render_confirm(view)

      assert_receive {:file_system_deleted, ^file_system}
      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "File storage deleted successfully"
      refute render(element(view, "#hub-file-systems-list")) =~ file_system.bucket_url
      refute file_system in Livebook.Hubs.get_file_systems(hub)
    end

    test "creates a deployment group", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      deployment_group =
        build(:deployment_group,
          name: "TEAM_ADD_DEPLOYMENT_GROUP",
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

      view
      |> element("#add-deployment-group")
      |> render_click()

      assert_patch(view, ~p"/hub/#{hub.id}/deployment-groups/new")

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
                      %DeploymentGroup{id: id, name: "TEAM_ADD_DEPLOYMENT_GROUP"} =
                        deployment_group}

      assert_patch(view, "/hub/#{hub.id}/deployment-groups/edit/#{id}")
      assert render(view) =~ "Deployment group TEAM_ADD_DEPLOYMENT_GROUP added successfully"
      assert deployment_group in Livebook.Teams.get_deployment_groups(hub)

      # Guarantee it shows the error from API

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}/deployment-groups/new")

      view
      |> element("#deployment-groups-form")
      |> render_submit(attrs)

      assert render(view) =~ "has already been taken"
    end

    test "updates an existing deployment group", %{conn: conn, hub: hub} do
      name = "TEAM_EDIT_DEPLOYMENT_GROUP"
      mode = :online
      insert_deployment_group(name: name, mode: mode, hub_id: hub.id)

      assert_receive {:deployment_group_created,
                      %DeploymentGroup{name: ^name, mode: ^mode, agent_keys: [_]} =
                        deployment_group}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{
        deployment_group: %{
          id: deployment_group.id,
          name: deployment_group.name,
          mode: deployment_group.mode,
          hub_id: deployment_group.hub_id
        }
      }

      new_name = "FOO"

      view
      |> element("#hub-deployment-group-#{deployment_group.id}-edit")
      |> render_click(%{"deployment_group_name" => deployment_group.id})

      assert_patch(view, ~p"/hub/#{hub.id}/deployment-groups/edit/#{deployment_group.id}")

      {:ok, view, html} =
        live(conn, ~p"/hub/#{hub.id}/deployment-groups/edit/#{deployment_group.id}")

      assert html =~ "Edit deployment group"

      assert html =~
               "Manage the #{deployment_group.name} (#{deployment_group.mode}) deployment group"

      view
      |> element("#deployment-groups-form")
      |> render_change(attrs)

      refute view
             |> element("#deployment-groups-form button[disabled]")
             |> has_element?()

      view
      |> element("#deployment-groups-form")
      |> render_submit(put_in(attrs.deployment_group.name, new_name))

      updated_deployment_group = %{deployment_group | name: new_name}

      assert_receive {:deployment_group_updated, ^updated_deployment_group}
      assert_patch(view, "/hub/#{hub.id}/deployment-groups/edit/#{deployment_group.id}")
      assert render(view) =~ "Deployment group #{new_name} updated successfully"
      assert updated_deployment_group in Livebook.Teams.get_deployment_groups(hub)
    end

    test "raises an error if the deployment group does not exist", %{conn: conn, hub: hub} do
      assert_raise LivebookWeb.NotFoundError, fn ->
        live(conn, ~p"/hub/#{hub.id}/deployment-groups/edit/9999999")
      end
    end
  end

  describe "agent" do
    setup %{node: node} do
      Livebook.Hubs.Broadcasts.subscribe([:crud, :connection])
      {agent_key, org, deployment_group, hub} = create_agent_team_hub(node)
      id = hub.id

      assert_receive {:hub_changed, ^id}
      assert_receive {:hub_connected, ^id}

      {:ok, hub: hub, agent_key: agent_key, org: org, deployment_group: deployment_group}
    end

    test "shows an error when creating a secret", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      secret = build(:secret, name: "TEAM_ADD_SECRET", hub_id: hub.id)

      attrs = %{
        secret: %{
          name: secret.name,
          value: secret.value,
          hub_id: secret.hub_id
        }
      }

      refute render(view) =~ secret.name

      view
      |> element("#add-secret")
      |> render_click(%{})

      assert_patch(view, ~p"/hub/#{hub.id}/secrets/new")
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

      refute_receive {:secret_created, ^secret}

      assert render(view) =~
               "You are not authorized to perform this action, make sure you have the access or you are not in a Livebook Agent instance"

      refute secret in Livebook.Hubs.get_secrets(hub)
    end

    test "shows an error when creating a file system", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      bypass = Bypass.open()
      file_system = build_bypass_file_system(bypass, hub.id)
      id = file_system.id
      attrs = %{file_system: Livebook.FileSystem.dump(file_system)}

      expect_s3_listing(bypass)

      refute render(view) =~ file_system.bucket_url

      view
      |> element("#add-file-system")
      |> render_click(%{})

      assert_patch(view, ~p"/hub/#{hub.id}/file-systems/new")
      assert render(view) =~ "Add file storage"

      view
      |> element("#file-systems-form")
      |> render_change(attrs)

      refute view
             |> element("#file-systems-form button[disabled]")
             |> has_element?()

      view
      |> element("#file-systems-form")
      |> render_submit(attrs)

      refute_receive {:file_system_created, %{id: ^id}}

      assert render(view) =~
               "You are not authorized to perform this action, make sure you have the access or you are not in a Livebook Agent instance"

      refute file_system in Livebook.Hubs.get_file_systems(hub)
    end
  end

  defp expect_s3_listing(bypass) do
    Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/xml")
      |> Plug.Conn.resp(200, """
      <ListBucketResult>
        <Name>mybucket</Name>
      </ListBucketResult>
      """)
    end)
  end

  defmodule Global do
    use Livebook.TeamsIntegrationCase, async: false

    setup %{user: user, node: node} do
      Livebook.Hubs.Broadcasts.subscribe([:crud, :connection, :secrets, :file_systems])
      hub = create_team_hub(user, node)
      id = hub.id

      assert_receive {:hub_connected, ^id}

      {:ok, hub: hub}
    end

    test "marking and unmarking hub as default", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      view
      |> element("button", "Mark as default")
      |> render_click()

      assert view
             |> element("span", "Default")
             |> has_element?()

      assert Hubs.get_default_hub().id == hub.id

      view
      |> element("button", "Remove as default")
      |> render_click()

      refute view
             |> element("span", "Default")
             |> has_element?()
    end
  end
end
