defmodule LivebookWeb.Integration.AppSessionLiveTest do
  use Livebook.TeamsIntegrationCase, async: false

  import Phoenix.LiveViewTest

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
    test "shows app if user doesn't have full access",
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
      pid = deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
      session_id = Livebook.App.get_session_id(pid, user: Livebook.Users.User.new())

      {:ok, _view, html} = live(conn, ~p"/apps/#{slug}/sessions/#{session_id}")
      assert html =~ "LivebookApp:#{slug}"
    end

    @tag :tmp_dir
    test "shows app if user have full access",
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
        pid = deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
        session_id = Livebook.App.get_session_id(pid, user: Livebook.Users.User.new())

        {:ok, _view, html} = live(conn, ~p"/apps/#{slug}/sessions/#{session_id}")
        assert html =~ "LivebookApp:#{slug}"
      end
    end

    @tag :tmp_dir
    test "renders unauthorized if loses the access in real-time",
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

      slug = "mkt-analytics"
      pid = deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
      session_id = Livebook.App.get_session_id(pid, user: Livebook.Users.User.new())
      path = ~p"/apps/#{slug}/sessions/#{session_id}"

      {:ok, view, _html} = live(conn, path)
      assert render(view) =~ "LivebookApp:#{slug}"

      erpc_call(node, :update_authorization_group, [authorization_group, %{prefixes: ["ops-"]}])

      id = to_string(deployment_group.id)

      assert_receive {:server_authorization_updated, %{id: ^id}}
      assert_receive {:app_deployment_updated, %{slug: ^slug}}
      assert_redirect view, path

      {:ok, view, _html} = live(conn, path)
      assert render(view) =~ "Not authorized"
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

    pid
  end
end
