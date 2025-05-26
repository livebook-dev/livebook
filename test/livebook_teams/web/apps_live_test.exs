defmodule LivebookWeb.Integration.AppsLiveTest do
  use Livebook.TeamsIntegrationCase, async: false

  import Phoenix.LiveViewTest

  @moduletag workspace_for: :agent
  setup :workspace

  @moduletag subscribe_to_hubs_topics: [:connection]
  @moduletag subscribe_to_teams_topics: [
               :clients,
               :agents,
               :deployment_groups,
               :app_deployments,
               :app_server
             ]

  setup do
    Livebook.Apps.subscribe()
    :ok
  end

  describe "authorized apps" do
    setup :livebook_teams_auth

    @tag :tmp_dir
    test "shows one app if user doesn't have full access",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      TeamsRPC.toggle_groups_authorization(node, context.deployment_group)
      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          prefixes: ["dev-"],
          oidc_provider: oidc_provider,
          deployment_group: context.deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slug = "dev-app"

      deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)

      html =
        conn
        |> get(~p"/apps")
        |> html_response(200)

      refute html =~ "No apps running."
      assert html =~ slug
    end

    @tag :tmp_dir
    test "shows all apps if user have full access",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      TeamsRPC.toggle_groups_authorization(node, context.deployment_group)
      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :app_server,
          oidc_provider: oidc_provider,
          deployment_group: context.deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slugs = ~w(mkt-app sales-app opt-app)

      for slug <- slugs do
        deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
      end

      html =
        conn
        |> get(~p"/apps")
        |> html_response(200)

      refute html =~ "No apps running."

      for slug <- slugs do
        assert html =~ slug
      end
    end

    @tag :tmp_dir
    test "updates the apps list in real-time",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      {:ok, %{groups_auth: true} = deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

      id = to_string(deployment_group.id)
      assert_receive {:deployment_group_updated, %{id: ^id, groups_auth: true}}

      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          prefixes: ["mkt-"],
          oidc_provider: oidc_provider,
          deployment_group: deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slug = "marketing-report"
      deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)

      {:ok, view, _} = live(conn, ~p"/apps")
      refute render(view) =~ slug

      {:ok, %{groups_auth: false}} = TeamsRPC.toggle_groups_authorization(node, deployment_group)
      assert_receive {:server_authorization_updated, %{id: ^id, groups_auth: false}}, 3_000

      {:ok, view, _} = live(conn, ~p"/apps")
      assert render(view) =~ slug
    end

    @tag :tmp_dir
    test "shows all apps if disable the authentication in real-time",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      {:ok, %{groups_auth: true} = deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

      id = to_string(deployment_group.id)
      assert_receive {:deployment_group_updated, %{id: ^id, groups_auth: true}}

      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          prefixes: ["mkt-"],
          oidc_provider: oidc_provider,
          deployment_group: deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slug = "marketing-app"
      deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)

      {:ok, view, _} = live(conn, ~p"/apps")
      refute render(view) =~ slug

      {:ok, %{teams_auth: false}} = TeamsRPC.toggle_teams_authentication(node, deployment_group)
      assert_receive {:server_authorization_updated, %{id: ^id, teams_auth: false}}, 3_000

      {:ok, view, _} = live(conn, ~p"/apps")
      assert render(view) =~ slug
    end
  end

  defp deploy_app(slug, team, org, deployment_group, tmp_dir, node) do
    source = """
    <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{team.id}","deployment_group_id":"#{deployment_group.id}"} -->

    # LivebookApp:#{slug}

    ```elixir
    ```
    """

    {notebook, %{warnings: []}} = Livebook.LiveMarkdown.notebook_from_livemd(source)

    files_dir = Livebook.FileSystem.File.local(tmp_dir)

    {:ok, %Livebook.Teams.AppDeployment{file: zip_content} = app_deployment} =
      Livebook.Teams.AppDeployment.new(notebook, files_dir)

    secret_key = Livebook.Teams.derive_key(team.teams_key)
    encrypted_content = Livebook.Teams.encrypt(zip_content, secret_key)

    app_deployment =
      TeamsRPC.upload_app_deployment(
        node,
        org,
        deployment_group,
        app_deployment,
        encrypted_content,
        # broadcast?
        true
      )

    app_deployment_id = to_string(app_deployment.id)
    assert_receive {:app_deployment_started, %{id: ^app_deployment_id}}

    assert_receive {:app_created, %{pid: pid, slug: ^slug}}, 50000

    assert_receive {:app_updated,
                    %{
                      slug: ^slug,
                      sessions: [%{app_status: %{execution: :executed, lifecycle: :active}}]
                    }},
                   5000

    on_exit(fn ->
      if Process.alive?(pid) do
        Livebook.App.close(pid)
      end
    end)
  end
end
