defmodule LivebookWeb.Integration.AppSessionLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Livebook.AppHelpers
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

      slug = "dev-oban-app"
      context = change_to_user_session(context)
      deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)

      change_to_agent_session(context)
      pid = wait_livebook_app_start(slug)
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

      slugs = [
        "mkt-livebook-app-#{Livebook.Utils.random_short_id()}",
        "eng-livebook-app-#{Livebook.Utils.random_short_id()}",
        "ops-livebook-app-#{Livebook.Utils.random_short_id()}"
      ]

      context = change_to_user_session(context)

      for slug <- slugs do
        deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
      end

      change_to_agent_session(context)

      for slug <- slugs do
        pid = wait_livebook_app_start(slug)
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

      slug = "mkt-analytics-#{Livebook.Utils.random_short_id()}"
      context = change_to_user_session(context)
      deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)

      change_to_agent_session(context)
      pid = wait_livebook_app_start(slug)
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

      slug = "analytics-app-#{Livebook.Utils.random_short_id()}"
      context = change_to_user_session(context)
      deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)

      change_to_agent_session(context)
      pid = wait_livebook_app_start(slug)
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
end
