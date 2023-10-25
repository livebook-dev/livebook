defmodule LivebookWeb.Integration.Hub.EditLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.Hubs

  setup %{user: user, node: node} do
    Livebook.Hubs.subscribe([:crud, :connection, :secrets, :file_systems])
    hub = create_team_hub(user, node)
    id = hub.id

    assert_receive {:hub_connected, ^id}

    {:ok, hub: hub}
  end

  describe "team" do
    test "updates the hub", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{"hub_emoji" => "🐈"}

      view
      |> element("#team-form")
      |> render_change(%{"team" => attrs})

      refute view
             |> element("#team-form .invalid-feedback")
             |> has_element?()

      assert {:ok, view, _html} =
               view
               |> element("#team-form")
               |> render_submit(%{"team" => attrs})
               |> follow_redirect(conn)

      assert render(view) =~ "Hub updated successfully"

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

      %{"success" => "Secret TEAM_ADD_SECRET added successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert secret in Livebook.Hubs.get_secrets(hub)
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

      %{"success" => "Secret TEAM_EDIT_SECRET updated successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
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
      |> element("#hub-secret-#{secret.name}-delete", "Delete")
      |> render_click()

      render_confirm(view)

      assert_receive {:secret_deleted, ^secret}

      %{"success" => "Secret TEAM_DELETE_SECRET deleted successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
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

      %{"success" => "File storage added successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

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

      %{"success" => "File storage updated successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

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

      %{"success" => "File storage deleted successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      refute render(element(view, "#hub-file-systems-list")) =~ file_system.bucket_url
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
      Livebook.Hubs.subscribe([:crud, :connection, :secrets, :file_systems])
      hub = create_team_hub(user, node)
      id = hub.id

      assert_receive {:hub_connected, ^id}

      {:ok, hub: hub}
    end

    test "marking and unmarking hub as default", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      assert {:ok, view, _html} =
               view
               |> element("a", "Mark as default")
               |> render_click()
               |> follow_redirect(conn)

      assert view
             |> element("span", "Default")
             |> has_element?()

      assert Hubs.get_default_hub().id == hub.id

      assert {:ok, view, _html} =
               view
               |> element("a", "Remove as default")
               |> render_click()
               |> follow_redirect(conn)

      refute view
             |> element("span", "Default")
             |> has_element?()
    end
  end
end
