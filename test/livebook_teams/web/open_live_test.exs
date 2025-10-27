defmodule LivebookWeb.Integration.OpenLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest

  @moduletag teams_for: :agent
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection, :file_systems]
  @moduletag subscribe_to_teams_topics: [
               :clients,
               :agents,
               :deployment_groups,
               :app_deployments,
               :app_server
             ]

  describe "git file storage" do
    @describetag :git

    setup %{test: test, team: team} do
      Livebook.FileSystem.Mounter.subscribe(team.id)
      data = test |> to_string() |> Base.encode32(padding: false, case: :lower)

      file_system =
        build(:fs_git,
          id: Livebook.FileSystem.Utils.id("git", team.id, data),
          hub_id: team.id,
          external_id: nil
        )

      {:ok, file_system: file_system}
    end

    test "lists files and folder on read-only mode",
         %{conn: conn, team: team, node: node, org_key: org_key, file_system: file_system} do
      file_system = TeamsRPC.create_file_system(node, team, org_key, file_system)
      assert_receive {:file_system_created, ^file_system}, 5_000
      assert_receive {:file_system_mounted, ^file_system}, 15_000

      {:ok, view, html} = live(conn, ~p"/open/storage")
      assert html =~ file_system.repo_url

      # change to Git file system
      view
      |> element(~s{button[id*="file-system-#{file_system.id}"]})
      |> render_click()

      # guarantee the write functions are disabled
      assert has_element?(view, ~s{div[id*="new-item-menu"] button[disabled]})
      assert render(view) =~ "notebook_files"

      # change the path to list the .livemd file
      view
      |> element(~s{form[id*="path-form"]})
      |> render_change(%{path: "/notebook_files/"})

      # render the view separately to make sure it received the :set_file event
      assert render(view) =~ "notebook.livemd"

      # select the file
      file_info_id = Base.url_encode64("/notebook_files/notebook.livemd", padding: false)

      view
      |> element(~s{div[id*="file-#{file_info_id}"] button[aria-label="notebook.livemd"]})
      |> render_click()

      # guarantee the open function is disabled
      assert has_element?(view, ~s{button[phx-click="open"][disabled]})

      # only fork is available
      assert has_element?(view, ~s{button[phx-click="fork"]:not([disabled])})
    end

    test "updates the list of file systems when receive events",
         %{conn: conn, team: team, node: node, org_key: org_key, file_system: file_system} do
      {:ok, view, _html} = live(conn, ~p"/open/storage")
      refute has_element?(view, ~s{button[id*="file-system-#{file_system.id}"]})

      file_system = TeamsRPC.create_file_system(node, team, org_key, file_system)
      assert_receive {:file_system_created, ^file_system}
      assert_receive {:file_system_mounted, ^file_system}, 15_000
      assert has_element?(view, ~s{button[id*="file-system-#{file_system.id}"]})

      file_system = %{file_system | branch: "test"}
      {:ok, _} = TeamsRPC.update_file_system(node, team, org_key, file_system)
      assert_receive {:file_system_updated, ^file_system}
      assert_receive {:file_system_mounted, ^file_system}, 15_000
      assert has_element?(view, ~s{button[id*="file-system-#{file_system.id}"]})

      TeamsRPC.delete_file_system(node, org_key, file_system.external_id)
      assert_receive {:file_system_deleted, ^file_system}
      assert_receive {:file_system_unmounted, ^file_system}, 15_000
      refute has_element?(view, ~s{button[id*="file-system-#{file_system.id}"]})
    end
  end
end
