defmodule Livebook.Integration.AppsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Apps

  describe "integration" do
    @tag :tmp_dir
    test "deploying apps with hub secrets", %{user: user, node: node, tmp_dir: tmp_dir} do
      Livebook.Hubs.Broadcasts.subscribe([:secrets])

      hub = create_team_hub(user, node)
      hub_id = hub.id
      secret = insert_secret(name: "APP_DB_PASSWORD", value: "postgres", hub_id: hub.id)
      secret_name = secret.name
      slug = Livebook.Utils.random_short_id()

      assert_receive {:secret_created, %{hub_id: ^hub_id, name: ^secret_name}}

      app_path = Path.join(tmp_dir, "app.livemd")

      source = """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{hub_id}"} -->

      # Team Hub

      ## Fetch Env Var

      ```elixir
      System.fetch_env!("LB_#{secret.name}")
      ```
      """

      {notebook, %{warnings: []}} = Livebook.LiveMarkdown.notebook_from_livemd(source)
      notebook = Map.replace!(notebook, :hub_secret_names, [secret_name])
      {source, []} = Livebook.LiveMarkdown.notebook_to_livemd(notebook)
      File.write!(app_path, source)

      Apps.subscribe()

      assert [app_spec] = Apps.build_app_specs_in_dir(tmp_dir)

      deployer_pid = Apps.Deployer.local_deployer()
      Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:app_created, %{pid: pid, slug: ^slug}}

      assert_receive {:app_updated,
                      %{
                        slug: ^slug,
                        sessions: [
                          %{app_status: %{execution: :executed, lifecycle: :active}} = session
                        ]
                      }}

      assert %{secrets: %{^secret_name => ^secret}} = Livebook.Session.get_data(session.pid)

      Livebook.App.close(pid)
    end
  end
end
