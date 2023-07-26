defmodule Livebook.Integration.AppsTest do
  use Livebook.TeamsIntegrationCase, async: true

  describe "deploy_apps_in_dir/1" do
    @tag :tmp_dir
    test "deploys apps with hub secrets", %{user: user, node: node, tmp_dir: tmp_dir} do
      hub = create_team_hub(user, node)
      hub_id = hub.id
      app_path = Path.join(tmp_dir, "app.livemd")
      secret = insert_secret(name: "DB_PASSWORD", value: "postgres", hub_id: hub.id)
      secret_name = secret.name

      markdown = """
      <!-- livebook:{"app_settings":{"slug":"#{hub_id}"},"hub_id":"#{hub_id}"} -->

      # Team Hub

      ## Fetch Env Var

      ```elixir
      System.fetch_env!("LB_#{secret.name}")
      ```
      """

      {notebook, []} = Livebook.LiveMarkdown.notebook_from_livemd(markdown)
      notebook = Map.replace!(notebook, :hub_secret_names, [secret_name])
      {source, []} = Livebook.LiveMarkdown.notebook_to_livemd(notebook)

      File.write!(app_path, source)

      Livebook.Apps.subscribe()
      Livebook.Apps.deploy_apps_in_dir(tmp_dir)

      assert_receive {:app_created, %{pid: pid, slug: ^hub_id}}

      assert_receive {:app_updated,
                      %{slug: ^hub_id, sessions: [%{app_status: %{execution: :executed}}]}}

      session_id = Livebook.App.get_session_id(pid)
      {:ok, session} = Livebook.Sessions.fetch_session(session_id)

      assert %{notebook: %{hub_secret_names: [^secret_name]}, hub_secrets: [^secret]} =
               Livebook.Session.get_data(session.pid)

      Livebook.App.close(pid)
    end
  end
end
