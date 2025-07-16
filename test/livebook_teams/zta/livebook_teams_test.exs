defmodule Livebook.ZTA.LivebookTeamsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.ZTA.LivebookTeams

  @moduletag teams_for: :agent
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection]
  @moduletag subscribe_to_teams_topics: [:clients, :agents]

  describe "authenticate/3" do
    setup %{team: team, test: test} do
      Livebook.Apps.subscribe()
      start_supervised!({LivebookTeams, name: test, identity_key: team.id})

      :ok
    end

    test "renders HTML with JavaScript redirect", %{conn: conn, test: test} do
      conn = init_test_session(conn, %{})
      assert {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert conn.halted
      assert html_response(conn, 200) =~ "window.location.href = "
    end

    test "gets the user information from Livebook Teams", %{conn: conn, node: node, test: test} do
      # Step 1: Get redirected to Livebook Teams
      conn = init_test_session(conn, %{})
      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      [_, location] = Regex.run(~r/URL\("(.*?)"\)/, html_response(conn, 200))
      uri = URI.parse(location)
      assert uri.path == "/identity/authorize"
      assert %{"token" => token} = URI.decode_query(uri.query)

      %{code: code} = TeamsRPC.allow_auth_request(node, token)

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
    setup :livebook_teams_auth

    test "revoke access token from Livebook Teams", %{conn: conn, test: test} do
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
end
