defmodule LivebookWeb.Hub.EditLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.HubHelpers
  import Livebook.TestHelpers

  alias Livebook.Hubs

  describe "personal" do
    setup tags do
      Livebook.Hubs.Broadcasts.subscribe([:crud, :secrets, :file_systems])
      hub = Hubs.fetch_hub!(Hubs.Personal.id())

      if tags[:git] do
        Livebook.FileSystem.Mounter.subscribe(hub.id)
      end

      {:ok, hub: hub}
    end

    test "updates the hub", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{"hub_emoji" => "🐈"}

      view
      |> element("#personal-form")
      |> render_change(%{"personal" => attrs})

      refute view
             |> element("#personal-form .invalid-feedback")
             |> has_element?()

      assert {:ok, view, _html} =
               view
               |> element("#personal-form")
               |> render_submit(%{"personal" => attrs})
               |> follow_redirect(conn)

      assert render(view) =~ "Workspace updated successfully"

      id = hub.id
      assert_receive {:hub_changed, ^id}

      assert_sidebar_hub(view, id, hub.hub_name, attrs["hub_emoji"])
      refute Hubs.fetch_hub!(hub.id) == hub

      Hubs.save_hub(hub)
    end

    test "raises an error if does not exist secret", %{conn: conn, hub: hub} do
      assert_raise LivebookWeb.NotFoundError, fn ->
        live(conn, ~p"/hub/#{hub.id}/secrets/edit/HELLO")
      end
    end

    test "creates secret", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      secret = build(:secret)

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
    end

    test "updates secret", %{conn: conn, hub: hub} do
      secret = insert_secret()

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

    test "deletes secret", %{conn: conn, hub: hub} do
      secret = insert_secret()

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

    test "creates a S3 file system", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      bypass = Bypass.open()
      file_system = build_bypass_file_system(bypass)

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

      assert_receive {:file_system_created, ^file_system}
      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "File storage added successfully"
      assert render(element(view, "#hub-file-systems-list")) =~ file_system.bucket_url
      assert file_system in Livebook.Hubs.get_file_systems(hub)
    end

    @tag :git
    test "creates a Git file system", %{conn: conn, hub: hub} do
      file_system = build(:fs_git)
      id = file_system.id

      # When the user paste the ssh key to the password input, it will remove the break lines.
      # So, the changeset need to normalize it before persisting. That said, the ssh key from
      # factory must be "denormalized" so we can test the user scenario.
      file_system = %{
        file_system
        | key: file_system.key |> String.replace("\n", "\s") |> String.trim()
      }

      attrs = %{file_system: Livebook.FileSystem.dump(file_system)}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      refute render(view) =~ file_system.id

      view
      |> element("#add-file-system")
      |> render_click()

      assert_patch(view, ~p"/hub/#{hub.id}/file-systems/new")
      assert render(view) =~ "Add file storage"

      # change the file system type from S3 to git
      view
      |> element("#file_system_type-git")
      |> render_click()

      view
      |> element("#file-systems-form")
      |> render_change(attrs)

      refute view
             |> element("#file-systems-form button[disabled]")
             |> has_element?()

      view
      |> element("#file-systems-form")
      |> render_submit(attrs)

      assert_receive {:file_system_created, %Livebook.FileSystem.Git{id: ^id} = file_system}
      assert_receive {:file_system_mounted, ^file_system}, 10_000

      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "File storage added successfully"
      assert render(element(view, "#hub-file-systems-list")) =~ file_system.id
      assert file_system in Livebook.Hubs.get_file_systems(hub)
    end

    test "updates existing S3 file system", %{conn: conn, hub: hub} do
      bypass = Bypass.open()
      file_system = build_bypass_file_system(bypass)
      :ok = Hubs.create_file_system(hub, file_system)

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

    @tag :git
    test "updates existing Git file system", %{test: test, conn: conn, hub: hub} do
      id = to_string(test)

      file_system =
        build(:fs_git,
          id: "git-#{String.replace(id, " ", "-")}-#{Livebook.Utils.random_short_id()}"
        )

      :ok = Hubs.create_file_system(hub, file_system)
      assert_receive {:file_system_created, %Livebook.FileSystem.Git{} = ^file_system}
      assert_receive {:file_system_mounted, ^file_system}, 10_000

      # guarantee the branch is "main" and "file.txt" exists 
      {:ok, paths} = Livebook.FileSystem.list(file_system, "/", false)
      assert "/file.txt" in paths
      refute "/another_file.txt" in paths

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      attrs = %{file_system: Livebook.FileSystem.dump(file_system)}
      attrs = put_in(attrs.file_system.branch, "test")

      view
      |> element("#hub-file-system-#{file_system.id}-edit")
      |> render_click(%{"file_system" => file_system})

      assert_patch(view, ~p"/hub/#{hub.id}/file-systems/edit/#{file_system.id}")
      assert render(view) =~ "Edit file storage"
      assert has_element?(view, "#file_system_type-git")

      view
      |> element("#file-systems-form")
      |> render_change(attrs)

      refute view
             |> element("#file-systems-form button[disabled]")
             |> has_element?()

      refute view
             |> element("#file-systems-form")
             |> render_submit(attrs) =~ "Connection test failed"

      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "File storage updated successfully"

      updated_file_system = %{file_system | branch: "test"}
      assert_receive {:file_system_updated, ^updated_file_system}
      assert_receive {:file_system_mounted, ^updated_file_system}, 10_000

      assert render(element(view, "#hub-file-systems-list")) =~ file_system.id
      assert updated_file_system in Livebook.Hubs.get_file_systems(hub)

      # guarantee the branch has changed and the repository is updated
      {:ok, paths} = Livebook.FileSystem.list(updated_file_system, "/", false)
      refute "/file.txt" in paths
      assert "/another_file.txt" in paths

      assert Livebook.FileSystem.unmount(file_system) == :ok
    end

    test "detaches existing S3 file system", %{conn: conn, hub: hub} do
      bypass = Bypass.open()
      file_system = build_bypass_file_system(bypass)
      :ok = Hubs.create_file_system(hub, file_system)

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

    @tag :git
    test "detaches existing Git file system", %{test: test, conn: conn, hub: hub} do
      id = to_string(test)

      file_system =
        build(:fs_git,
          id: "git-#{String.replace(id, " ", "-")}-#{Livebook.Utils.random_short_id()}"
        )

      :ok = Hubs.create_file_system(hub, file_system)
      assert_receive {:file_system_created, %Livebook.FileSystem.Git{} = ^file_system}
      assert_receive {:file_system_mounted, ^file_system}, 10_000

      # guarantee the folder exists
      repo_dir = Livebook.FileSystem.Git.git_dir(file_system)
      assert File.exists?(repo_dir)

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      view
      |> element("#hub-file-system-#{file_system.id}-detach", "Detach")
      |> render_click()

      render_confirm(view)

      assert_receive {:file_system_deleted, ^file_system}
      assert_receive {:file_system_unmounted, ^file_system}, 10_000

      assert_patch(view, "/hub/#{hub.id}")
      assert render(view) =~ "File storage deleted successfully"
      refute render(element(view, "#hub-file-systems-list")) =~ file_system.id
      refute file_system in Livebook.Hubs.get_file_systems(hub)

      # guarantee the folder were deleted
      refute File.exists?(repo_dir)
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
end
