defmodule LivebookWeb.Integration.AdminLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest

  @moduletag teams_for: :agent
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection]
  @moduletag subscribe_to_teams_topics: [:clients, :agents, :app_deployments, :app_server]

  describe "authorization" do
    setup :livebook_teams_auth

    test "renders unauthorized admin page if user doesn't have full access",
         %{conn: conn, node: node, code: code} = context do
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

      assert conn
             |> get(~p"/settings")
             |> html_response(401) =~ "Not authorized"
    end

    test "shows admin page if user have full access",
         %{conn: conn, node: node, code: code} = context do
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

      {:ok, _view, html} = live(conn, ~p"/settings")
      assert html =~ "System settings"
    end

    test "renders unauthorized if loses the access in real-time",
         %{conn: conn, node: node, code: code} = context do
      {:ok, deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :app_server,
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

      {:ok, view, _html} = live(conn, ~p"/settings")
      assert render(view) =~ "System settings"

      TeamsRPC.update_authorization_group(node, authorization_group, %{
        access_type: :apps,
        prefixes: ["ops-"]
      })

      id = to_string(deployment_group.id)
      assert_receive {:server_authorization_updated, %{id: ^id}}

      # If you lose access to the app server, we will redirect to "/"
      assert_redirect view, ~p"/"

      # And it will redirect to "/apps"
      {:ok, view, _html} = live(conn, ~p"/apps")
      assert render(view) =~ "Apps"
    end

    test "shows admin page if authentication is disabled",
         %{conn: conn, node: node, code: code} = context do
      {:ok, deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          prefixes: ["ops-"],
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

      assert conn
             |> get(~p"/settings")
             |> html_response(401) =~ "Not authorized"

      {:ok, %{teams_auth: false} = deployment_group} =
        TeamsRPC.toggle_teams_authentication(node, deployment_group)

      id = to_string(deployment_group.id)
      assert_receive {:server_authorization_updated, %{id: ^id, teams_auth: false}}

      {:ok, view, _html} = live(conn, ~p"/settings")
      assert render(view) =~ "System settings"
    end
  end

  describe "topbar" do
    setup %{teams_auth: teams_auth, conn: conn} do
      {:ok, conn: init_test_session(conn, %{teams_auth_test_override: teams_auth})}
    end

    for page <- ["/", "/open", "/settings", "/learn", "/hub", "/apps-dashboard"] do
      @tag page: page, teams_auth: :online
      test "GET #{page} shows the app server instance topbar warning", %{conn: conn, page: page} do
        {:ok, view, _} = live(conn, page)

        assert render(view) =~
                 "This Livebook instance has been configured for notebook deployment and is in read-only mode."
      end

      @tag page: page, teams_auth: :offline
      test "GET #{page} shows the offline hub topbar warning", %{conn: conn, page: page} do
        {:ok, view, _} = live(conn, page)

        assert render(view) =~
                 "You are running an offline Workspace for deployment. You cannot modify its settings."
      end
    end
  end
end
