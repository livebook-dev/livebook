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
      # Step 1: Would get redirected to Livebook to check if it's a bot
      conn = init_test_session(conn, %{})
      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert html_response(conn, 200) =~ "teams_redirect"

      session = get_session(conn)
      assert state = session["teams_auth_state"]

      redirect_to =
        LivebookWeb.Endpoint.url()
        |> URI.new!()
        |> URI.append_query(URI.encode_query(%{"teams_identity" => "", "teams_state" => state}))
        |> URI.to_string()

      # Step 2: Checks if the given request belongs to a browser
      conn =
        build_conn(:get, "/", %{teams_redirect: "", redirect_to: redirect_to})
        |> init_test_session(session)

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      # Step 3: Get redirected to Livebook Teams
      location = Phoenix.ConnTest.redirected_to(conn)
      uri = URI.parse(location)
      assert uri.path == "/identity/authorize"
      assert %{"token" => token, "redirect_to" => ^redirect_to} = URI.decode_query(uri.query)

      %{code: code} = TeamsRPC.allow_auth_request(node, token)

      # Step 4: Emulate the redirect back with the code for validation
      conn =
        build_conn(:get, "/", %{teams_identity: "", teams_state: state, code: code})
        |> init_test_session(session)

      assert {conn, %{id: _id, name: _, email: _, payload: %{}} = metadata} =
               LivebookTeams.authenticate(test, conn, [])

      assert redirected_to(conn, 302) == "/"

      # Step 5: Confirm the token is valid for future requests
      conn =
        build_conn(:get, "/")
        |> init_test_session(Plug.Conn.get_session(conn))

      assert {%{halted: false}, ^metadata} = LivebookTeams.authenticate(test, conn, [])
    end

    test "does not accept a code obtained in another authentication flow",
         %{conn: conn, node: node, test: test} do
      # Someone goes through the authentication flow themselves, up to
      # the point where they have a code for their own identity
      attacker_conn = init_test_session(conn, %{})
      {attacker_conn, nil} = LivebookTeams.authenticate(test, attacker_conn, [])
      assert attacker_state = get_session(attacker_conn, :teams_auth_state)

      redirect_to =
        LivebookWeb.Endpoint.url()
        |> URI.new!()
        |> URI.append_query(
          URI.encode_query(%{"teams_identity" => "", "teams_state" => attacker_state})
        )

      attacker_conn =
        build_conn(:get, "/", %{teams_redirect: "", redirect_to: URI.to_string(redirect_to)})
        |> init_test_session(get_session(attacker_conn))

      {attacker_conn, nil} = LivebookTeams.authenticate(test, attacker_conn, [])

      uri = attacker_conn |> Phoenix.ConnTest.redirected_to() |> URI.parse()
      assert %{"token" => token} = URI.decode_query(uri.query)

      %{code: code} = TeamsRPC.allow_auth_request(node, token)

      # Meanwhile the victim visits Livebook and starts their own flow
      victim_conn = init_test_session(conn, %{})
      {victim_conn, nil} = LivebookTeams.authenticate(test, victim_conn, [])
      victim_session = get_session(victim_conn)

      # Making the victim's browser complete the flow with the code has
      # no effect, no matter which state it is presented with
      for params <- [
            %{teams_identity: "", teams_state: attacker_state, code: code},
            %{teams_identity: "", code: code}
          ] do
        conn =
          build_conn(:get, "/", params)
          |> init_test_session(victim_session)

        assert {conn, nil} = LivebookTeams.authenticate(test, conn, [])
        assert redirected_to(conn, 302) == "/"
        refute get_session(conn, :livebook_teams_access_token)

        # Following the redirect starts the authentication flow over
        conn =
          build_conn(:get, "/")
          |> init_test_session(get_session(conn))

        assert {conn, nil} = LivebookTeams.authenticate(test, conn, [])
        assert html_response(conn, 200) =~ "teams_redirect"
      end
    end

    test "shows an error when the user does not belong to the org", %{conn: conn, test: test} do
      # Step 1: Start the authentication flow, which stores the state in the session
      conn = init_test_session(conn, %{})
      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert state = get_session(conn, :teams_auth_state)

      # Step 2: Emulate a request coming from Teams saying the user does not belong to the org
      params_from_teams = %{
        "teams_identity" => "",
        "teams_state" => state,
        "failed_reason" => "you do not belong to this org"
      }

      conn =
        build_conn(:get, "/", params_from_teams)
        |> init_test_session(get_session(conn))

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert conn.status == 302

      # Step 3: follow the redirect keeping the session set in previous request
      conn =
        build_conn(:get, redirected_to(conn))
        |> init_test_session(get_session(conn))

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      assert html_response(conn, 403) =~
               "Failed to authenticate with Livebook Teams: you do not belong to this org"
    end

    test "starts over when the callback carries neither code nor failure reason",
         %{conn: conn, test: test} do
      conn = init_test_session(conn, %{})
      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert state = get_session(conn, :teams_auth_state)

      conn =
        build_conn(:get, "/", %{teams_identity: "", teams_state: state})
        |> init_test_session(get_session(conn))

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert redirected_to(conn, 302) == "/"
    end

    test "ignores a failure reason from an unknown authentication flow",
         %{conn: conn, test: test} do
      conn = init_test_session(conn, %{})
      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      params_from_teams = %{
        "teams_identity" => "",
        "teams_state" => "invalid",
        "failed_reason" => "you do not belong to this org"
      }

      conn =
        build_conn(:get, "/", params_from_teams)
        |> init_test_session(get_session(conn))

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert redirected_to(conn, 302) == "/"
      refute get_session(conn, :teams_failed_reason)
    end

    test "deletes the cache if access token is invalid",
         %{test: test, node: node, team: team} do
      {conn, code} = authenticate_user_on_teams(test, node, team)
      access_token = get_session(conn, :livebook_teams_access_token)
      metadata_node = get_session(conn, :livebook_teams_metadata_node)

      TeamsRPC.revoke_auth_request(node, code)
      assert :erpc.call(metadata_node, :ets, :lookup_element, [test, access_token, 2, nil])

      assert {%{halted: true} = conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert html_response(conn, 200) =~ "window.location.href = "
      refute :erpc.call(metadata_node, :ets, :lookup_element, [test, access_token, 2, nil])
    end

    test "shows unsupported version error page with older livebook version",
         %{test: test, node: node, team: team} do
      {conn, _code} = authenticate_user_on_teams(test, node, team)

      # forces the min version enforcement
      pid = Livebook.Hubs.TeamClient.get_pid(team.id)
      :sys.replace_state(pid, &Map.replace(&1, :version_enforcement, "0.15.6"))

      assert {%{halted: true} = conn, nil} = LivebookTeams.authenticate(test, conn, [])

      assert html_response(conn, 503) =~
               "This Livebook version is no longer compatible with Livebook Teams. Please update this app server to 0.15.6 or later to restore access."
    end

    @tag subscribe_to_teams_topics: [:clients, :agents, :deployment_groups]
    test "shows pending connection error page once the deployment group is deleted",
         %{conn: conn, node: node, test: test, deployment_group: deployment_group} do
      id = to_string(deployment_group.id)
      TeamsRPC.delete_deployment_group(node, deployment_group)
      assert_receive {:deployment_group_deleted, %{id: ^id}}

      conn = init_test_session(conn, %{})

      assert {%{halted: true} = conn, nil} = LivebookTeams.authenticate(test, conn, [])

      assert html_response(conn, 503) =~
               "This Livebook instance cannot be accessed because it has not yet established a connection to Livebook Teams."
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

  defmodule Global do
    # We need to "turn off" the Teams API during test
    use Livebook.TeamsIntegrationCase, async: false

    alias Livebook.ZTA.LivebookTeams

    @moduletag :capture_log
    @moduletag teams_for: :agent
    setup :teams

    @moduletag subscribe_to_hubs_topics: [:connection]
    @moduletag subscribe_to_teams_topics: [:clients, :agents]

    setup :livebook_teams_auth

    test "uses cached version of the identity payload", %{conn: conn, test: test} = ctx do
      Application.put_env(:livebook, :teams_url, "http://localhost:1234")

      id = conn.assigns.current_user.id
      access_token = get_session(conn, :livebook_teams_access_token)
      groups = [%{"provider_id" => "1", "group_name" => "Foo"}]
      node = get_session(conn, :livebook_teams_metadata_node)

      # update the groups, but doesn't return because Livebook is using the cached one
      TeamsRPC.update_user_info_groups(ctx.node, ctx.code, groups)

      # shouldn't retry the request
      current_timestamp = System.os_time(:second)
      assert {_, %{id: ^id, groups: []}} = LivebookTeams.authenticate(test, conn, [])
      assert System.os_time(:second) - current_timestamp < :timer.seconds(1)

      # simulate if the token already expired
      exp = System.os_time(:second) - 5 * 60
      {_, metadata} = :erpc.call(node, :ets, :lookup_element, [test, access_token, 2, nil])
      :erpc.call(node, :ets, :insert, [test, {access_token, {exp, metadata}}])

      # now it should retry to request to Teams and return status 503
      assert ExUnit.CaptureLog.capture_log(fn ->
               assert {%{status: 503, halted: true, resp_body: body}, nil} =
                        LivebookTeams.authenticate(test, conn, [])

               assert body =~ "The server is currently down or under maintenance"
             end) =~ "retry: got exception, will retry in"

      # now gets the updated userinfo from Teams
      Application.put_env(:livebook, :teams_url, TeamsServer.url())
      assert {_conn, %{id: ^id, groups: ^groups}} = LivebookTeams.authenticate(test, conn, [])
    end
  end

  defmodule PendingConnection do
    # No TeamsIntegrationCase needed — we test the scenario where
    # the TeamClient has never connected to Teams
    use LivebookWeb.ConnCase, async: true

    alias Livebook.ZTA.LivebookTeams

    setup %{conn: conn, test: test} do
      team = build(:team, user_id: nil)
      Livebook.Hubs.save_hub(team)
      on_exit(fn -> Livebook.Hubs.delete_hub(team.id) end)

      start_supervised!({LivebookTeams, name: test, identity_key: team.id})

      {:ok, conn: conn, team: team}
    end

    test "returns 503 when Teams connection is pending", %{conn: conn, test: test} do
      conn = init_test_session(conn, %{})

      assert {conn, nil} = LivebookTeams.authenticate(test, conn, [])
      assert conn.halted
      assert conn.status == 503
      assert conn.resp_body =~ "it has not yet established a connection to Livebook Teams"
    end
  end
end
