defmodule LivebookWeb.Integration.Hub.EditLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.Hubs

  describe "user" do
    setup %{user: user, node: node} do
      Livebook.Hubs.Broadcasts.subscribe([:crud, :connection, :secrets, :file_systems])
      Livebook.Teams.Broadcasts.subscribe([:clients, :deployment_groups])
      hub = create_team_hub(user, node)
      id = hub.id

      assert_receive {:hub_connected, ^id}
      assert_receive {:client_connected, ^id}

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
      assert update =~ "Workspace updated successfully"
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
      |> element("#delete-hub", "Delete workspace")
      |> render_click()

      render_confirm(view)

      assert_receive {:hub_changed, ^id}
      %{"success" => "Workspace deleted successfully"} = assert_redirect(view, "/")

      {:ok, view, _html} = live(conn, ~p"/")

      refute_sidebar_hub(view, id)
      assert_raise Livebook.Storage.NotFoundError, fn -> Hubs.fetch_hub!(id) end
    end

    test "creates a secret", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      secret = build(:secret, hub_id: hub.id)

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
      assert render(view) =~ "Secret #{secret.name} added successfully"
      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert secret in Livebook.Hubs.get_secrets(hub)

      # Guarantee it shows the error from API
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}/secrets/new")

      assert view
             |> element("#secrets-form")
             |> render_submit(attrs) =~ "has already been taken"
    end

    test "updates existing secret", %{conn: conn, hub: hub} do
      secret = insert_secret(hub_id: hub.id)

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
      |> element("#hub-secrets-list [aria-label=\"edit #{secret.name}\"]")
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
      assert render(view) =~ "Secret #{secret.name} updated successfully"
      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert updated_secret in Livebook.Hubs.get_secrets(hub)
    end

    test "deletes existing secret", %{conn: conn, hub: hub} do
      secret = insert_secret(hub_id: hub.id)
      assert_receive {:secret_created, ^secret}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      refute view
             |> element("#secrets-form button[disabled]")
             |> has_element?()

      view
      |> element("#hub-secrets-list [aria-label=\"delete #{secret.name}\"]")
      |> render_click()

      render_confirm(view)

      assert_receive {:secret_deleted, ^secret}
      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "Secret #{secret.name} deleted successfully"
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
      secret = build(:secret, hub_id: hub.id)

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

      assert view
             |> element("#secrets-form")
             |> render_submit(attrs) =~
               "You are not authorized to perform this action, make sure you have the access and you are not in a Livebook App Server/Offline instance"

      refute secret in Livebook.Hubs.get_secrets(hub)
    end

    test "shows an error when creating a file system", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      bypass = Bypass.open()
      file_system = build_bypass_file_system(bypass, hub.id)
      attrs = %{file_system: Livebook.FileSystem.dump(file_system)}

      expect_s3_listing(bypass)
      refute render(view) =~ file_system.bucket_url

      view
      |> element("#add-file-system")
      |> render_click(%{})

      assert_patch(view, ~p"/hub/#{hub.id}/file-systems/new")
      assert render(view) =~ "Add file storage"

      assert view
             |> element("#file-systems-form")
             |> render_submit(attrs) =~
               "You are not authorized to perform this action, make sure you have the access and you are not in a Livebook App Server/Offline instance"

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
    # Not async, because we alter global config (default hub)
    use Livebook.TeamsIntegrationCase, async: false

    setup %{user: user, node: node} do
      Livebook.Hubs.Broadcasts.subscribe([:crud, :connection, :secrets, :file_systems])
      Livebook.Teams.Broadcasts.subscribe([:clients])
      hub = create_team_hub(user, node)
      id = hub.id

      assert_receive {:hub_connected, ^id}
      assert_receive {:client_connected, ^id}

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
