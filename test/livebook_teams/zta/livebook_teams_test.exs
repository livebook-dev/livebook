defmodule Livebook.ZTA.LivebookTeamsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.ZTA.LivebookTeams

  setup %{test: test, node: node} do
    Livebook.Teams.Broadcasts.subscribe([:agents])
    {_agent_key, org, deployment_group, team} = create_agent_team_hub(node)

    # we wait until the agent_connected is received by livebook
    hub_id = team.id
    deployment_group_id = to_string(deployment_group.id)
    org_id = to_string(org.id)

    assert_receive {:agent_joined,
                    %{hub_id: ^hub_id, org_id: ^org_id, deployment_group_id: ^deployment_group_id}}

    start_supervised!({LivebookTeams, name: test, identity_key: team.id})
    {:ok, deployment_group: deployment_group, org: org, team: team}
  end

  describe "authenticate/3" do
    test "renders HTML with JavaScript redirect", %{conn: conn, test: test} do
      conn = init_test_session(conn, %{})
      assert {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert conn.halted
      assert html_response(conn, 200) =~ "window.location.href = "
    end

    test "gets the user information from Livebook Teams",
         %{conn: conn, node: node, test: test} do
      # Step 1: Get redirected to Livebook Teams
      conn = init_test_session(conn, %{})
      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      [_, location] = Regex.run(~r/URL\("(.*?)"\)/, html_response(conn, 200))
      uri = URI.parse(location)
      assert uri.path == "/identity/authorize"
      assert %{"code" => code} = URI.decode_query(uri.query)

      erpc_call(node, :allow_auth_request, [code])

      # Step 2: Emulate the redirect back with the code for validation
      conn =
        build_conn(:get, "/", %{teams_identity: "", code: code})
        |> init_test_session(Plug.Conn.get_session(conn))

      assert {conn, %{id: _id, name: _, email: _, payload: %{}} = metadata} =
               LivebookTeams.authenticate(test, conn, [])

      assert redirected_to(conn, 302) == "/"

      # Step 3: Confirm the token is valid for future requests
      conn =
        build_conn(:get, "/")
        |> init_test_session(Plug.Conn.get_session(conn))

      assert {%{halted: false}, ^metadata} = LivebookTeams.authenticate(test, conn, [])
    end

    test "authorizes user to access admin page with full access permission",
         %{conn: conn, node: node, deployment_group: deployment_group, org: org, test: test} do
      erpc_call(node, :toggle_groups_authorization, [deployment_group])
      oidc_provider = erpc_call(node, :create_oidc_provider, [org])

      authorization_group =
        erpc_call(node, :create_authorization_group, [
          %{
            group_name: "developers",
            access_type: :app_server,
            oidc_provider: oidc_provider,
            deployment_group: deployment_group
          }
        ])

      {conn, code, %{restricted_apps_groups: []}} = authenticate_user(conn, node, test)
      session = get_session(conn)

      conn =
        build_conn(:get, ~p"/")
        |> init_test_session(session)

      group = %{
        "provider_id" => to_string(oidc_provider.id),
        "group_name" => authorization_group.group_name
      }

      # Get the user with updated groups
      erpc_call(node, :update_user_info_groups, [code, [group]])

      assert {%{halted: false}, %{restricted_apps_groups: nil}} =
               LivebookTeams.authenticate(test, conn, [])
    end

    @tag :tmp_dir
    test "renders unauthorized user to access app with prefix the user don't have access",
         %{
           conn: conn,
           node: node,
           deployment_group: deployment_group,
           org: org,
           tmp_dir: tmp_dir,
           team: team,
           test: test
         } do
      erpc_call(node, :toggle_groups_authorization, [deployment_group])
      oidc_provider = erpc_call(node, :create_oidc_provider, [org])

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

      Livebook.Apps.subscribe()
      slug = "marketing-app"

      notebook = %{
        Livebook.Notebook.new()
        | app_settings: %{Livebook.Notebook.AppSettings.new() | slug: slug},
          file_entries: [],
          name: slug,
          hub_id: team.id,
          deployment_group_id: to_string(deployment_group.id)
      }

      files_dir = Livebook.FileSystem.File.local(tmp_dir)

      {:ok, %Livebook.Teams.AppDeployment{file: zip_content} = app_deployment} =
        Livebook.Teams.AppDeployment.new(notebook, files_dir)

      secret_key = Livebook.Teams.derive_key(team.teams_key)
      encrypted_content = Livebook.Teams.encrypt(zip_content, secret_key)

      Livebook.Teams.Broadcasts.subscribe(:app_deployments)

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

      Livebook.Apps.Manager.sync_permanent_apps()
      Livebook.Apps.subscribe()

      assert_receive {:app_created, %{pid: pid, slug: ^slug}}

      assert_receive {:app_updated,
                      %{
                        slug: ^slug,
                        sessions: [%{app_status: %{execution: :executed, lifecycle: :active}}]
                      }}

      # Now we need to check if the current user has access to this app
      {conn, code, %{restricted_apps_groups: []}} = authenticate_user(conn, node, test)
      session = get_session(conn)

      group = %{
        "provider_id" => to_string(oidc_provider.id),
        "group_name" => authorization_group.group_name
      }

      # Update user groups
      erpc_call(node, :update_user_info_groups, [code, [group]])

      # Guarantee we don't list the app for this user
      conn = build_conn(:get, ~p"/") |> init_test_session(session)
      {_conn, metadata} = LivebookTeams.authenticate(test, conn, [])
      {:ok, user} = Livebook.Users.update_user(Livebook.Users.User.new(metadata.id), metadata)
      assert Livebook.Apps.list_authorized_apps(user) == []

      Livebook.App.close(pid)
    end

    test "renders unauthorized user to access admin page with slug prefix access permission",
         %{conn: conn, node: node, deployment_group: deployment_group, org: org, test: test} do
      erpc_call(node, :toggle_groups_authorization, [deployment_group])
      oidc_provider = erpc_call(node, :create_oidc_provider, [org])

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

      {conn, code, %{restricted_apps_groups: []}} = authenticate_user(conn, node, test)

      group = %{
        "provider_id" => to_string(oidc_provider.id),
        "group_name" => authorization_group.group_name
      }

      erpc_call(node, :update_user_info_groups, [code, [group]])

      conn =
        build_conn(:get, ~p"/settings")
        |> init_test_session(get_session(conn))

      assert {_conn, %{restricted_apps_groups: [^group]}} =
               LivebookTeams.authenticate(test, conn, [])
    end

    test "redirects to Livebook Teams with invalid access token",
         %{conn: conn, test: test} do
      conn = init_test_session(conn, %{livebook_teams_access_token: "1234567890"})
      assert {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert conn.halted
      assert html_response(conn, 200) =~ "window.location.href = "
    end

    test "shows an error when the user does not belong to the org", %{conn: conn, test: test} do
      # Step 1: Emulate a request coming from Teams saying the user does belong to the org
      conn = init_test_session(conn, %{})

      params_from_teams = %{
        "teams_identity" => "",
        "failed_reason" => "you do not belong to this org"
      }

      conn = %Plug.Conn{conn | params: params_from_teams}

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert conn.status == 302

      # Step 2: follow the redirect keeping the session set in previous request
      conn =
        build_conn(:get, redirected_to(conn))
        |> init_test_session(get_session(conn))

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      assert html_response(conn, 403) =~
               "Failed to authenticate with Livebook Teams: you do not belong to this org"
    end
  end

  describe "logout/2" do
    test "revoke access token from Livebook Teams", %{conn: conn, node: node, test: test} do
      {conn, _code, metadata} = authenticate_user(conn, node, test)
      assert {%{halted: false}, ^metadata} = LivebookTeams.authenticate(test, conn, [])

      # Revoke the token and the metadata will be invalid for future requests
      assert %{status: 302} = conn = LivebookTeams.logout(test, conn)
      [url] = get_resp_header(conn, "location")
      assert %{status: 200} = Req.get!(url)

      # If we try to authenticate again, it should redirect to Teams
      conn =
        build_conn(:get, ~p"/")
        |> init_test_session(get_session(conn))

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert conn.halted
      assert html_response(conn, 200) =~ "window.location.href = "
    end
  end

  defp authenticate_user(conn, node, test) do
    conn = init_test_session(conn, %{})
    {conn, nil} = LivebookTeams.authenticate(test, conn, [])

    [_, location] = Regex.run(~r/URL\("(.*?)"\)/, html_response(conn, 200))
    uri = URI.parse(location)
    %{"code" => code} = URI.decode_query(uri.query)

    erpc_call(node, :allow_auth_request, [code])

    session = get_session(conn)

    conn =
      build_conn(:get, ~p"/", %{teams_identity: "", code: code})
      |> init_test_session(session)

    {conn, %{id: _id} = metadata} = LivebookTeams.authenticate(test, conn, [])
    session = get_session(conn)

    {init_test_session(build_conn(), session), code, metadata}
  end
end
