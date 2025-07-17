defmodule LivebookCLI.Integration.DeployTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Livebook.AppHelpers

  alias Livebook.Utils
  alias LivebookCLI.Deploy

  @url "http://localhost:4200"

  @moduletag teams_for: :user
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection]
  @moduletag subscribe_to_teams_topics: [:clients, :deployment_groups, :app_deployments]

  @moduletag :tmp_dir
  @moduletag :capture_io

  describe "CLI deploy integration" do
    test "successfully deploys a notebook via CLI",
         %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      title = "Test CLI Deploy App"
      slug = Utils.random_short_id()
      app_path = Path.join(tmp_dir, "#{slug}.livemd")
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)
      hub_id = team.id
      deployment_group_id = to_string(deployment_group.id)

      stamp_notebook(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{hub_id}"} -->

      # #{title}

      ## Test Section

      ```elixir
      IO.puts("Hello from CLI deployed app!")
      ```
      """)

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          assert Deploy.call([
                   "--deploy-key",
                   key,
                   "--teams-key",
                   team.teams_key,
                   "--deployment-group",
                   deployment_group.name,
                   app_path
                 ]) == :ok
        end)

      assert output =~ "App deployment created successfully."
      assert output =~ "#{slug} (#{@url}/apps/#{slug})"
      assert output =~ title

      assert_receive {:app_deployment_started,
                      %{
                        title: ^title,
                        slug: ^slug,
                        deployment_group_id: ^deployment_group_id,
                        hub_id: ^hub_id,
                        deployed_by: "CLI"
                      }}
    end

    test "successfully deploys multiple notebooks from directory",
         %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)
      hub_id = team.id
      deployment_group_id = to_string(deployment_group.id)

      apps =
        for i <- 1..3 do
          title = "Test App #{i}"
          slug = "app-#{i}-#{Utils.random_short_id()}"
          app_path = Path.join(tmp_dir, "app_#{i}.livemd")

          stamp_notebook(app_path, """
          <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{hub_id}"} -->

          # #{title}

          ```elixir
          IO.puts("Hello from app #{i}!")
          ```
          """)

          {slug, title}
        end

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          assert Deploy.call([
                   "--deploy-key",
                   key,
                   "--teams-key",
                   team.teams_key,
                   "--deployment-group",
                   deployment_group.name,
                   tmp_dir
                 ]) == :ok
        end)

      for {slug, title} <- apps do
        assert output =~ "App deployment created successfully."
        assert output =~ "#{slug} (#{@url}/apps/#{slug})"
        assert output =~ title

        assert_receive {:app_deployment_started,
                        %{
                          title: ^title,
                          slug: ^slug,
                          deployment_group_id: ^deployment_group_id,
                          hub_id: ^hub_id,
                          deployed_by: "CLI"
                        }}
      end
    end

    test "fails with invalid deploy key", %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      slug = Utils.random_short_id()
      app_path = Path.join(tmp_dir, "#{slug}.livemd")
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)

      stamp_notebook(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{team.id}"} -->

      # Test App
      """)

      assert_raise RuntimeError, ~r/Deploy Key.*must be a Livebook Teams Deploy Key/s, fn ->
        Deploy.call([
          "--deploy-key",
          "invalid_key",
          "--teams-key",
          team.teams_key,
          "--deployment-group",
          deployment_group.name,
          app_path
        ])
      end
    end

    test "fails with invalid teams key", %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      slug = Utils.random_short_id()
      app_path = Path.join(tmp_dir, "#{slug}.livemd")
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)

      stamp_notebook(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{team.id}"} -->

      # Test App
      """)

      assert_raise RuntimeError, ~r/Teams Key.*must be a Livebook Teams Key/s, fn ->
        Deploy.call([
          "--deploy-key",
          key,
          "--teams-key",
          "invalid-key",
          "--deployment-group",
          deployment_group.name,
          app_path
        ])
      end
    end

    test "fails with missing deployment group",
         %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      slug = Utils.random_short_id()
      app_path = Path.join(tmp_dir, "#{slug}.livemd")
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)

      stamp_notebook(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{team.id}"} -->

      # Test App
      """)

      assert_raise RuntimeError, ~r/Deployment Group.*can't be blank/s, fn ->
        Deploy.call([
          "--deploy-key",
          key,
          "--teams-key",
          team.teams_key,
          app_path
        ])
      end
    end

    test "fails with non-existent file", %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)

      assert_raise RuntimeError, ~r/Path.*must be a valid path/s, fn ->
        Deploy.call([
          "--deploy-key",
          key,
          "--teams-key",
          team.teams_key,
          "--deployment-group",
          deployment_group.name,
          Path.join(tmp_dir, "app.livemd")
        ])
      end
    end

    test "fails when directory contains no notebooks",
         %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)

      File.write!(Path.join(tmp_dir, "readme.txt"), "No notebooks here")
      File.write!(Path.join(tmp_dir, "config.json"), "{}")

      assert_raise RuntimeError, ~r/There's no notebook available to deploy/, fn ->
        Deploy.call([
          "--deploy-key",
          key,
          "--teams-key",
          team.teams_key,
          "--deployment-group",
          deployment_group.name,
          tmp_dir
        ])
      end
    end
  end
end
