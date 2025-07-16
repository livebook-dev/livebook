defmodule Livebook.Integration.AppsTest do
  use Livebook.TeamsIntegrationCase, async: true

  @moduletag teams_for: :user
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection, :secrets]
  @moduletag subscribe_to_teams_topics: [:clients, :agents]

  describe "integration" do
    @tag :tmp_dir
    test "deploying apps with hub secrets",
         %{team: team, org_key: org_key, node: node, tmp_dir: tmp_dir} do
      hub_id = team.id
      secret = TeamsRPC.create_secret(node, team, org_key)
      secret_name = secret.name
      secret_value = secret.value

      assert_receive {:secret_created,
                      %{hub_id: ^hub_id, name: ^secret_name, value: ^secret_value}}

      slug = Livebook.Utils.random_short_id()
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

      Livebook.Apps.subscribe()

      assert [app_spec] = Livebook.Apps.build_app_specs_in_dir(tmp_dir)

      deployer_pid = Livebook.Apps.Deployer.local_deployer()
      Livebook.Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:app_created, %{pid: pid, slug: ^slug}}

      assert_receive {:app_updated,
                      %{
                        slug: ^slug,
                        sessions: [
                          %{app_status: %{execution: :executed, lifecycle: :active}} = session
                        ]
                      }}

      assert %{
               secrets: %{
                 ^secret_name => %Livebook.Secrets.Secret{
                   name: ^secret_name,
                   value: ^secret_value,
                   deployment_group_id: nil,
                   hub_id: ^hub_id
                 }
               }
             } = Livebook.Session.get_data(session.pid)

      Livebook.App.close(pid)
    end
  end
end
