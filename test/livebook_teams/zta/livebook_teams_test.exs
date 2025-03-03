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
    {:ok, deployment_group: deployment_group, team: team}
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
      session = Plug.Conn.get_session(conn)

      assert conn.status == 302

      # Step 2: follow the redirect keeping the session set in previous request
      conn = build_conn(:get, redirected_to(conn))
      conn = init_test_session(conn, session)

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      assert html_response(conn, 200) =~
               "Failed to authenticate with Livebook Teams: you do not belong to this org"
    end
  end

  describe "logout/2" do
    test "revoke access token from Livebook Teams", %{conn: conn, node: node, test: test} do
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

      # Step 4: Revoke the token and the metadata will be invalid for future requests
      conn =
        build_conn(:get, "/")
        |> init_test_session(Plug.Conn.get_session(conn))

      assert LivebookTeams.logout(test, conn) == :ok

      # Step 5: If we try to revoke again, it should fail
      assert {:error, _} = LivebookTeams.logout(test, conn)

      # Step 6: It we try to authenticate again, it should redirect to Teams
      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert conn.halted
      assert html_response(conn, 200) =~ "window.location.href = "
    end
  end
end
