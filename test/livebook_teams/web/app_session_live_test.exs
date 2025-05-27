defmodule LivebookWeb.Integration.AppSessionLiveTest do
  use Livebook.TeamsIntegrationCase, async: false

  import Phoenix.LiveViewTest

  @moduletag teams_for: :agent
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection]
  @moduletag subscribe_to_teams_topics: [:clients, :agents, :app_deployments, :app_server]

  setup do
    Livebook.Apps.subscribe()
    :ok
  end

  describe "authorized apps" do
    setup :livebook_teams_auth

    @tag :tmp_dir
    test "shows app if user doesn't have full access",
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
      pid = deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
      session_id = Livebook.App.get_session_id(pid, user: Livebook.Users.User.new())

      {:ok, _view, html} = live(conn, ~p"/apps/#{slug}/sessions/#{session_id}")
      assert html =~ "LivebookApp:#{slug}"
    end

    @tag :tmp_dir
    test "shows app if user have full access",
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

      slugs = ~w(mkt-livebook-app eng-livebook-app ops-livebook-app)

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
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

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

      slug = "mkt-analytics"
      pid = deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
      session_id = Livebook.App.get_session_id(pid, user: Livebook.Users.User.new())
      path = ~p"/apps/#{slug}/sessions/#{session_id}"

      {:ok, view, _html} = live(conn, path)
      assert render(view) =~ "LivebookApp:#{slug}"

      {:ok, %{prefixes: ["ops-"]}} =
        TeamsRPC.update_authorization_group(node, authorization_group, %{prefixes: ["ops-"]})

      id = to_string(deployment_group.id)

      assert_receive {:server_authorization_updated, %{id: ^id}}
      assert_receive {:app_deployment_updated, %{slug: ^slug}}
      assert_redirect view, path

      {:ok, view, _html} = live(conn, path)
      assert render(view) =~ "Not authorized"
    end

    @tag :tmp_dir
    test "shows app if disable the authentication in real-time",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      {:ok, deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

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

      slug = "analytics-app"
      pid = deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
      session_id = Livebook.App.get_session_id(pid, user: Livebook.Users.User.new())
      path = ~p"/apps/#{slug}/sessions/#{session_id}"

      {:ok, view, _html} = live(conn, path)
      assert render(view) =~ "Not authorized"

      {:ok, %{teams_auth: false} = deployment_group} =
        TeamsRPC.toggle_teams_authentication(node, deployment_group)

      id = to_string(deployment_group.id)
      assert_receive {:server_authorization_updated, %{id: ^id, teams_auth: false}}
      assert_redirect view, path

      {:ok, view, _html} = live(conn, path)
      assert render(view) =~ "LivebookApp:#{slug}"
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
      TeamsRPC.upload_app_deployment(
        node,
        org,
        deployment_group,
        app_deployment,
        encrypted_content,
        # broadcast?
        true
      ).id

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
