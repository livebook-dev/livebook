defmodule Livebook.Teams.ConnectionTest do
  use Livebook.TeamsIntegrationCase, async: true

  @moduletag :capture_log

  alias Livebook.Teams.Connection

  describe "connect" do
    test "successfully authenticates the websocket connection", %{user: user, node: node} do
      org = :erpc.call(node, Hub.Integration, :create_org, [])
      org_key = :erpc.call(node, Hub.Integration, :create_org_key, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      header = [
        {"x-user", to_string(user.id)},
        {"x-org", to_string(org.id)},
        {"x-org-key", to_string(org_key.id)},
        {"x-session-token", token}
      ]

      assert {:ok, _conn} = Connection.start_link(self(), header)
      assert_receive {:connect, :ok, :connected}
    end

    test "rejects the websocket connection with invalid credentials", %{user: user} do
      header = [
        {"x-user", to_string(user.id)},
        {"x-org", to_string(user.id)},
        {"x-org-key", to_string(user.id)},
        {"x-session-token", "foo"}
      ]

      assert {:ok, _conn} = Connection.start_link(self(), header)

      assert_receive {:connect, :error, reason}
      assert reason =~ "could not authenticate the user from given credentials"

      assert {:ok, _conn} = Connection.start_link(self(), [])

      assert_receive {:connect, :error, reason}
      assert reason =~ "could not find the request credentials"
    end
  end
end
