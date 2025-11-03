defmodule Livebook.Integration.LiveMarkdown.ImportTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Notebook
  alias Livebook.LiveMarkdown

  @moduletag teams_for: :user
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection]
  @moduletag subscribe_to_teams_topics: [:clients, :app_folders]

  describe "app settings" do
    test "don't import app folder if does not exists anymore",
         %{node: node, team: team, org: org} do
      app_folder = TeamsRPC.create_app_folder(node, name: "delete me", org: org)

      app_folder_id = to_string(app_folder.id)
      hub_id = team.id

      assert_receive {:app_folder_created, %{id: ^app_folder_id, hub_id: ^hub_id}}

      notebook = %{
        Notebook.new()
        | name: "Deleted from folder",
          hub_id: hub_id,
          app_settings: %{Notebook.AppSettings.new() | app_folder_id: app_folder_id},
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: []
            }
          ]
      }

      {markdown, []} = LiveMarkdown.Export.notebook_to_livemd(notebook)

      TeamsRPC.delete_app_folder(node, app_folder)
      assert_receive {:app_folder_deleted, %{id: ^app_folder_id, hub_id: ^hub_id}}

      assert {%Notebook{name: "Deleted from folder", app_settings: %{app_folder_id: nil}},
              %{warnings: warnings}} = LiveMarkdown.Import.notebook_from_livemd(markdown)

      assert "notebook is assigned to a non-existent app folder, defaulting to ungrouped app folder" in warnings
    end
  end
end
