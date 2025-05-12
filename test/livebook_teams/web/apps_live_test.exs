defmodule LivebookWeb.Integration.AppsLiveTest do
  use Livebook.TeamsIntegrationCase, async: false

  describe "authorized apps" do
    setup %{conn: conn, node: node} do
      Livebook.Teams.Broadcasts.subscribe([:agents, :app_deployments, :app_server])
      Livebook.Apps.subscribe()

      {_agent_key, org, deployment_group, team} = create_agent_team_hub(node)

      # we wait until the agent_connected is received by livebook
      hub_id = team.id
      deployment_group_id = to_string(deployment_group.id)
      org_id = to_string(org.id)

      assert_receive {:agent_joined,
                      %{
                        hub_id: ^hub_id,
                        org_id: ^org_id,
                        deployment_group_id: ^deployment_group_id
                      }}

      start_supervised!(
        {Livebook.ZTA.LivebookTeams, name: LivebookWeb.ZTA, identity_key: team.id}
      )

      {conn, code} = authenticate_user_on_teams(conn, node, team)

      {:ok, conn: conn, code: code, deployment_group: deployment_group, org: org, team: team}
    end

    @tag :tmp_dir
    test "shows one app if user doesn't have full access",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      erpc_call(node, :toggle_groups_authorization, [context.deployment_group])
      oidc_provider = erpc_call(node, :create_oidc_provider, [context.org])

      authorization_group =
        erpc_call(node, :create_authorization_group, [
          %{
            group_name: "marketing",
            access_type: :apps,
            prefixes: ["dev-"],
            oidc_provider: oidc_provider,
            deployment_group: context.deployment_group
          }
        ])

      erpc_call(node, :update_user_info_groups, [
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      ])

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
      erpc_call(node, :toggle_groups_authorization, [context.deployment_group])
      oidc_provider = erpc_call(node, :create_oidc_provider, [context.org])

      authorization_group =
        erpc_call(node, :create_authorization_group, [
          %{
            group_name: "marketing",
            access_type: :app_server,
            oidc_provider: oidc_provider,
            deployment_group: context.deployment_group
          }
        ])

      erpc_call(node, :update_user_info_groups, [
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      ])

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
      {:ok, deployment_group} =
        erpc_call(node, :toggle_groups_authorization, [context.deployment_group])

      oidc_provider = erpc_call(node, :create_oidc_provider, [context.org])

      authorization_group =
        erpc_call(node, :create_authorization_group, [
          %{
            group_name: "marketing",
            access_type: :apps,
            prefixes: ["mkt-"],
            oidc_provider: oidc_provider,
            deployment_group: deployment_group
          }
        ])

      erpc_call(node, :update_user_info_groups, [
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      ])

      slug = "marketing-app"

      deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)

      assert conn
             |> get(~p"/apps")
             |> html_response(200) =~ "No apps running."

      {:ok, %{groups_auth: false} = deployment_group} =
        erpc_call(node, :toggle_groups_authorization, [deployment_group])

      id = to_string(deployment_group.id)
      assert_receive {:server_authorization_updated, %{id: ^id, groups_auth: false}}

      assert conn
             |> get(~p"/apps")
             |> html_response(200) =~ slug
    end

    @tag :tmp_dir
    test "shows all apps if disable the authentication in real-time",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      {:ok, deployment_group} =
        erpc_call(node, :toggle_groups_authorization, [context.deployment_group])

      oidc_provider = erpc_call(node, :create_oidc_provider, [context.org])

      authorization_group =
        erpc_call(node, :create_authorization_group, [
          %{
            group_name: "marketing",
            access_type: :apps,
            prefixes: ["mkt-"],
            oidc_provider: oidc_provider,
            deployment_group: deployment_group
          }
        ])

      erpc_call(node, :update_user_info_groups, [
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      ])

      slug = "marketing-app"

      deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)

      assert conn
             |> get(~p"/apps")
             |> html_response(200) =~ "No apps running."

      {:ok, %{teams_auth: false} = deployment_group} =
        erpc_call(node, :toggle_teams_authentication, [deployment_group])

      id = to_string(deployment_group.id)
      assert_receive {:server_authorization_updated, %{id: ^id, teams_auth: false}}

      assert conn
             |> get(~p"/apps")
             |> html_response(200) =~ slug
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

    app_deployment_id =
      erpc_call(node, :upload_app_deployment, [
        org,
        deployment_group,
        app_deployment,
        encrypted_content,
        # broadcast?
        true
      ]).id

    app_deployment_id = to_string(app_deployment_id)
    assert_receive {:app_deployment_started, %{id: ^app_deployment_id}}

    assert_receive {:app_created, %{pid: pid, slug: ^slug}}

    assert_receive {:app_updated,
                    %{
                      slug: ^slug,
                      sessions: [%{app_status: %{execution: :executed, lifecycle: :active}}]
                    }}

    on_exit(fn ->
      if Process.alive?(pid) do
        Livebook.App.close(pid)
      end
    end)
  end
end
