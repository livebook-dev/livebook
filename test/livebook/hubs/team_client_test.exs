defmodule Livebook.Hubs.TeamClientTest do
  use Livebook.TeamsIntegrationCase, async: true
  @moduletag :capture_log

  alias Livebook.Hubs.TeamClient

  setup do
    Livebook.Hubs.subscribe([:connection])
    :ok
  end

  describe "start_link/1" do
    test "successfully authenticates the web socket connection", %{user: user, node: node} do
      org = :erpc.call(node, Hub.Integration, :create_org, [])
      org_key = :erpc.call(node, Hub.Integration, :create_org_key, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      team =
        build(:team,
          user_id: user.id,
          org_id: org.id,
          org_key_id: org_key.id,
          session_token: token
        )

      refute TeamClient.connected?(team.id)

      TeamClient.start_link(team)
      assert_receive :hub_connected
      assert TeamClient.connected?(team.id)
    end

    test "rejects the web socket connection with invalid credentials", %{user: user, token: token} do
      team =
        build(:team,
          user_id: user.id,
          org_id: 123_456,
          org_key_id: 123_456,
          session_token: token
        )

      TeamClient.start_link(team)

      assert_receive {:hub_server_error, error}

      assert error ==
               "#{team.hub_name}: Your session is out-of-date. Please re-join the organization."
    end
  end
end
