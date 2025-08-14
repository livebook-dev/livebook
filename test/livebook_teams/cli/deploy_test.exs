defmodule LivebookCLI.Integration.DeployTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Livebook.AppHelpers

  alias Livebook.Utils

  @url "http://localhost:4200"

  @moduletag teams_for: :user
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection]
  @moduletag subscribe_to_teams_topics: [:clients, :deployment_groups, :app_deployments]

  @moduletag :tmp_dir

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
          assert deploy(
                   key,
                   team.teams_key,
                   deployment_group.id,
                   app_path
                 ) == :ok
        end)

      assert output =~ "* Preparing to deploy notebook #{slug}.livemd"
      assert output =~ "  * #{title} deployed successfully. (#{@url}/apps/#{slug})"

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
          app_path = Path.join(tmp_dir, "#{slug}.livemd")

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
          assert deploy(
                   key,
                   team.teams_key,
                   deployment_group.id,
                   Path.join(tmp_dir, "*.livemd")
                 ) == :ok
        end)

      for {slug, title} <- apps do
        assert output =~ "* Preparing to deploy notebook #{slug}.livemd"
        assert output =~ "  * #{title} deployed successfully. (#{@url}/apps/#{slug})"

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

    test "fails with unauthorized deploy key",
         %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      title = "Test CLI Deploy App"
      slug = Utils.random_short_id()
      app_path = Path.join(tmp_dir, "#{slug}.livemd")
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)

      deployment_group =
        TeamsRPC.create_deployment_group(node, org: org, url: @url, deploy_auth: true)

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
          assert_raise(LivebookCLI.Error, "Some app deployments failed.", fn ->
            assert deploy(
                     key,
                     team.teams_key,
                     deployment_group.id,
                     app_path
                   ) == :ok
          end)
        end)

      assert output =~ "* Preparing to deploy notebook #{slug}.livemd"

      assert output =~
               "* Test CLI Deploy App failed to deploy. Transport error: You are not authorized to perform this action, make sure you have the access to deploy apps to this deployment group"

      refute_receive {:app_deployment_started,
                      %{
                        title: ^title,
                        slug: ^slug,
                        deployment_group_id: ^deployment_group_id,
                        hub_id: ^hub_id,
                        deployed_by: "CLI"
                      }}
    end

    test "fails with invalid deploy key", %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      slug = Utils.random_short_id()
      app_path = Path.join(tmp_dir, "#{slug}.livemd")
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)

      stamp_notebook(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{team.id}"} -->

      # Test App
      """)

      assert_raise LivebookCLI.Error, ~r/Deploy Key must be a Livebook Teams Deploy Key/s, fn ->
        deploy(
          "invalid_key",
          team.teams_key,
          deployment_group.id,
          app_path
        )
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

      assert_raise LivebookCLI.Error, ~r/Teams Key must be a Livebook Teams Key/s, fn ->
        deploy(
          key,
          "invalid-key",
          deployment_group.id,
          app_path
        )
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

      assert_raise OptionParser.ParseError,
                   ~r/--deployment-group-id : Expected type integer, got ""/s,
                   fn ->
                     deploy(
                       key,
                       team.teams_key,
                       "",
                       app_path
                     )
                   end
    end

    test "fails with invalid deployment group",
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
          assert_raise(LivebookCLI.Error, ~r/Some app deployments failed./s, fn ->
            deploy(
              key,
              team.teams_key,
              999_999,
              app_path
            )
          end)
        end)

      assert output =~ ~r/Deployment Group ID does not exist/

      refute_receive {:app_deployment_started,
                      %{
                        title: ^title,
                        slug: ^slug,
                        deployment_group_id: ^deployment_group_id,
                        hub_id: ^hub_id,
                        deployed_by: "CLI"
                      }}
    end

    test "fails with non-existent file", %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)

      assert_raise LivebookCLI.Error, ~r/File Paths must be a valid path/s, fn ->
        deploy(
          key,
          team.teams_key,
          deployment_group.id,
          Path.join(tmp_dir, "app.livemd")
        )
      end
    end

    test "fails with directory argument", %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      {key, _} = TeamsRPC.create_deploy_key(node, org: org)
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)

      assert_raise LivebookCLI.Error, ~r/File Paths must be a file path/s, fn ->
        deploy(
          key,
          team.teams_key,
          deployment_group.id,
          tmp_dir
        )
      end
    end

    test "handles partial failure when deploying multiple notebooks",
         %{team: team, node: node, org: org, tmp_dir: tmp_dir} do
      {deploy_key, _} = TeamsRPC.create_deploy_key(node, org: org)
      deployment_group = TeamsRPC.create_deployment_group(node, org: org, url: @url)
      hub_id = team.id

      # Notebook without app settings (deploymeny should fail)
      invalid_title = "Invalid App"
      invalid_slug = "invalid-#{Utils.random_short_id()}"
      invalid_app_path = Path.join(tmp_dir, "#{invalid_slug}.livemd")

      File.write!(invalid_app_path, """
      <!-- livebook:{"hub_id":"#{hub_id}"} -->

      # #{invalid_title}

      ```elixir
      1 + 1
      ```
      """)

      # Second notebook should succeed
      valid_title = "Valid App"
      valid_slug = "valid-#{Utils.random_short_id()}"
      valid_app_path = Path.join(tmp_dir, "#{valid_slug}.livemd")

      stamp_notebook(valid_app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{valid_slug}"},"hub_id":"#{hub_id}"} -->

      # #{valid_title}

      ```elixir
      1 + 1
      ```
      """)

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          assert_raise(LivebookCLI.Error, "Some app deployments failed.", fn ->
            deploy(
              deploy_key,
              team.teams_key,
              deployment_group.id,
              [invalid_app_path, valid_app_path]
            )
          end)
        end)

      # The valid notebook should have been deployed successfully
      assert output =~
               "#{valid_title} deployed successfully. (#{@url}/apps/#{valid_slug})"

      deployment_group_id = to_string(deployment_group.id)

      assert_receive {:app_deployment_started,
                      %{
                        title: ^valid_title,
                        slug: ^valid_slug,
                        deployment_group_id: ^deployment_group_id,
                        hub_id: ^hub_id,
                        deployed_by: "CLI"
                      }}

      # And deployment of the invalide notebook shows an error message
      invalid_app_filename = Path.basename(invalid_app_path)

      assert output =~
               "Deployment for notebook #{invalid_app_filename} failed"
    end
  end

  defp deploy(deploy_key, teams_key, deployment_group_id, path) do
    paths =
      if is_list(path) do
        path
      else
        case Path.wildcard(path) do
          [] -> [path]
          [path] -> [path]
          paths -> paths
        end
      end

    deployment_group_id =
      cond do
        deployment_group_id == "" -> ""
        true -> Integer.to_string(deployment_group_id)
      end

    LivebookCLI.Deploy.call(
      [
        "--deploy-key",
        deploy_key,
        "--teams-key",
        teams_key,
        "--deployment-group-id",
        deployment_group_id
      ] ++ paths
    )
  end
end
