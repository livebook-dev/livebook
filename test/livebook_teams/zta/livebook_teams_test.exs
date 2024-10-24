defmodule Livebook.ZTA.LivebookTeamsTest do
  # Not async, because we alter global config (default hub)
  use Livebook.TeamsIntegrationCase, async: false
  use Plug.Test

  alias Livebook.ZTA.LivebookTeams

  setup %{node: node} do
    {_agent_key, _org, deployment_group, team} = create_agent_team_hub(node)
    Livebook.Hubs.set_default_hub(team.id)

    {:ok, deployment_group: deployment_group, team: team}
  end

  test "fails to start when the default hub doesn't exists", %{test: test} do
    Livebook.Hubs.set_default_hub("personal-hub")
    assert_raise MatchError, fn -> LivebookTeams.start_link(name: test) end
  end

  describe "authenticate/3" do
    test "redirects the user to Livebook Teams OAuth2 page",
         %{conn: conn, node: node, test: test, deployment_group: deployment_group} do
      :ignore = LivebookTeams.start_link(name: test)
      conn = Plug.Test.init_test_session(conn, %{})

      erpc_call(node, :update_deployment_group, [
        deployment_group,
        %{zta_provider: :livebook_teams}
      ])

      assert {%{status: 302, halted: true}, nil} = LivebookTeams.authenticate(test, conn, [])
    end

    test "fails to request user authentication with invalid zta provider in the deployment group",
         %{conn: conn, test: test} do
      :ignore = LivebookTeams.start_link(name: test)
      conn = Plug.Test.init_test_session(conn, %{})

      assert {%{status: 400}, nil} = LivebookTeams.authenticate(test, conn, [])
    end

    test "gets the user information from Livebook Teams",
         %{conn: conn, node: node, test: test, deployment_group: deployment_group} do
      :ignore = LivebookTeams.start_link(name: test)
      conn = Plug.Test.init_test_session(conn, %{})

      erpc_call(node, :update_deployment_group, [
        deployment_group,
        %{zta_provider: :livebook_teams}
      ])

      {conn, nil} = LivebookTeams.authenticate(test, conn, [])

      [location] = get_resp_header(conn, "location")
      uri = URI.parse(location)
      assert uri.path === "/identity/authorize"

      redirect_to = LivebookWeb.Endpoint.url() <> "/"
      assert %{"code" => code, "redirect_to" => ^redirect_to} = URI.decode_query(uri.query)

      erpc_call(node, :allow_auth_request, [code])
      conn = conn(:get, "/", %{auth: "true", code: code}) |> Plug.Test.init_test_session(%{})

      assert {conn, %{id: _id, name: _, email: _, payload: %{"access_token" => _}} = metadata} =
               LivebookTeams.authenticate(test, conn, [])

      assert conn.status == 302
      assert get_resp_header(conn, "location") == ["/"]

      conn = Plug.Test.init_test_session(conn(:get, "/"), %{identity_data: metadata})
      assert {%{halted: false}, ^metadata} = LivebookTeams.authenticate(test, conn, [])
    end

    test "redirects to Livebook Teams with invalid access token",
         %{conn: conn, node: node, test: test, deployment_group: deployment_group} do
      identity_data = %{
        id: "11",
        name: "Ada Lovelace",
        payload: %{"access_token" => "1234567890"},
        email: "user95387220@example.com"
      }

      :ignore = LivebookTeams.start_link(name: test)
      conn = Plug.Test.init_test_session(conn, %{identity_data: identity_data})

      erpc_call(node, :update_deployment_group, [
        deployment_group,
        %{zta_provider: :livebook_teams}
      ])

      assert {%{status: 302}, nil} = LivebookTeams.authenticate(test, conn, [])
    end
  end
end
