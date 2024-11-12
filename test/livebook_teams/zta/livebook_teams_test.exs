defmodule Livebook.ZTA.LivebookTeamsTest do
  # Not async, because we alter global config (teams auth)
  use Livebook.TeamsIntegrationCase, async: false
  use Plug.Test

  alias Livebook.ZTA.LivebookTeams

  setup %{test: test, node: node} do
    Livebook.Teams.Broadcasts.subscribe([:agents])

    {_agent_key, org, deployment_group, team} =
      create_agent_team_hub(node, deployment_group: [zta_provider: :livebook_teams])

    # we wait until the agent_connected is received by livebook
    hub_id = team.id
    deployment_group_id = to_string(deployment_group.id)
    org_id = to_string(org.id)

    assert_receive {:agent_joined,
                    %{hub_id: ^hub_id, org_id: ^org_id, deployment_group_id: ^deployment_group_id}}

    {:ok,
     deployment_group: deployment_group, team: team, opts: [name: test, identity_key: team.id]}
  end

  describe "authenticate/3" do
    test "redirects the user to Livebook Teams for authentication",
         %{conn: conn, test: test, opts: opts} do
      start_supervised({LivebookTeams, opts})
      conn = Plug.Test.init_test_session(conn, %{})

      assert {%{status: 302, halted: true}, nil} = LivebookTeams.authenticate(test, conn, [])
    end

    test "gets the user information from Livebook Teams",
         %{conn: conn, node: node, test: test, opts: opts} do
      start_supervised({LivebookTeams, opts})
      conn = Plug.Test.init_test_session(conn, %{})
      conn = %{conn | host: "my-livebook.com"}
      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      [location] = get_resp_header(conn, "location")
      uri = URI.parse(location)
      assert uri.path == "/identity/authorize"

      redirect_to =  "http://my-livebook.com/?teams_identity"
      assert %{"code" => code, "redirect_to" => ^redirect_to} = URI.decode_query(uri.query)

      erpc_call(node, :allow_auth_request, [code])

      conn =
        conn(:get, "/", %{teams_identity: "", code: code})
        |> Plug.Test.init_test_session(%{})

      assert {conn, %{id: _id, name: _, email: _, payload: %{"access_token" => _}} = metadata} =
               LivebookTeams.authenticate(test, conn, [])

      assert conn.status == 302
      assert get_resp_header(conn, "location") == ["/"]

      conn = Plug.Test.init_test_session(conn(:get, "/"), %{identity_data: metadata})
      assert {%{halted: false}, ^metadata} = LivebookTeams.authenticate(test, conn, [])
    end

    test "redirects to Livebook Teams with invalid access token",
         %{conn: conn, test: test, opts: opts} do
      identity_data = %{
        id: "11",
        name: "Ada Lovelace",
        payload: %{"access_token" => "1234567890"},
        email: "user95387220@example.com"
      }

      start_supervised({LivebookTeams, opts})
      conn = Plug.Test.init_test_session(conn, %{identity_data: identity_data})

      assert {%{status: 302}, nil} = LivebookTeams.authenticate(test, conn, [])
    end
  end
end
