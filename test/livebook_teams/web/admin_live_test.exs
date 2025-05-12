defmodule LivebookWeb.Integration.AdminLiveTest do
  # Not async, because we alter global config (app server instance)
  use Livebook.TeamsIntegrationCase, async: false

  import Phoenix.LiveViewTest

  describe "topbar" do
    setup %{teams_auth: teams_auth} do
      Application.put_env(:livebook, :teams_auth, teams_auth)
      on_exit(fn -> Application.delete_env(:livebook, :teams_auth) end)

      :ok
    end

    for page <- ["/", "/settings", "/learn", "/hub", "/apps-dashboard"] do
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

  describe "authorization" do
    setup %{conn: conn, node: node} do
      Livebook.Teams.Broadcasts.subscribe([:agents, :app_server])
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

    test "renders unauthorized admin page if user doesn't have full access",
         %{conn: conn, node: node, code: code} = context do
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

      assert conn
             |> get(~p"/settings")
             |> html_response(401) =~ "Not authorized"
    end

    test "shows admin page if user have full access",
         %{conn: conn, node: node, code: code} = context do
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

      {:ok, _view, html} = live(conn, ~p"/settings")
      assert html =~ "System settings"
    end

    test "renders unauthorized if loses the access in real-time",
         %{conn: conn, node: node, code: code} = context do
      {:ok, deployment_group} =
        erpc_call(node, :toggle_groups_authorization, [context.deployment_group])

      oidc_provider = erpc_call(node, :create_oidc_provider, [context.org])

      authorization_group =
        erpc_call(node, :create_authorization_group, [
          %{
            group_name: "marketing",
            access_type: :app_server,
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

      {:ok, view, _html} = live(conn, ~p"/settings")
      assert render(view) =~ "System settings"

      erpc_call(node, :update_authorization_group, [
        authorization_group,
        %{access_type: :apps, prefixes: ["ops-"]}
      ])

      id = to_string(deployment_group.id)
      assert_receive {:server_authorization_updated, %{id: ^id}}

      # If you lose access to the app server, we will redirect to "/"
      assert_redirect view, ~p"/"

      # And it will redirect to "/apps"
      {:ok, view, _html} = live(conn, ~p"/apps")
      assert render(view) =~ "No apps running."
    end

    test "shows admin page if authentication is disabled",
         %{conn: conn, node: node, code: code} = context do
      {:ok, deployment_group} =
        erpc_call(node, :toggle_groups_authorization, [context.deployment_group])

      oidc_provider = erpc_call(node, :create_oidc_provider, [context.org])

      authorization_group =
        erpc_call(node, :create_authorization_group, [
          %{
            group_name: "marketing",
            access_type: :apps,
            prefixes: ["ops-"],
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

      assert conn
             |> get(~p"/settings")
             |> html_response(401) =~ "Not authorized"

      {:ok, %{teams_auth: false} = deployment_group} =
        erpc_call(node, :toggle_teams_authentication, [deployment_group])

      id = to_string(deployment_group.id)
      assert_receive {:server_authorization_updated, %{id: ^id, teams_auth: false}}

      {:ok, view, _html} = live(conn, ~p"/settings")
      assert render(view) =~ "System settings"
    end
  end
end
