defmodule Livebook.Teams.ConnectionTest do
  use Livebook.TeamsIntegrationCase, async: true

  @moduletag :capture_log

  alias Livebook.Teams.Connection

  describe "connect" do
    test "successfully authenticates the websocket connection", %{user: user, node: node} do
      org = :erpc.call(node, Hub.Integration, :create_org, [])
      org_key = :erpc.call(node, Hub.Integration, :create_org_key, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      headers = [
        {"x-user", to_string(user.id)},
        {"x-org", to_string(org.id)},
        {"x-org-key", to_string(org_key.id)},
        {"x-session-token", token}
      ]

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected
    end

    test "rejects the websocket connection with invalid credentials", %{user: user} do
      headers = [
        {"x-user", to_string(user.id)},
        {"x-org", to_string(user.id)},
        {"x-org-key", to_string(user.id)},
        {"x-session-token", "foo"}
      ]

      assert {:ok, _conn} = Connection.start_link(self(), headers)

      assert_receive {:server_error,
                      "Your session is out-of-date. Please re-join the organization."}

      assert {:ok, _conn} = Connection.start_link(self(), [])

      assert_receive {:server_error,
                      "Invalid request. Please re-join the organization and update Livebook if the issue persists."}
    end
  end

  describe "handle events" do
    test "receives the secret_created event", %{user: user, node: node} do
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
      assert_receive :connected

      # creates a new secret
      hub =
        build(:team,
          user_id: user.id,
          org_id: org.id,
          org_key_id: org_key.id,
          session_token: token
        )

      secret = build(:secret, name: "FOO", value: "BAR")
      assert Livebook.Teams.create_secret(hub, secret) == :ok

      # receives `{:event, :secret_created, secret_created}` event
      # without decrypting the value
      assert_receive {:event, :secret_created, secret_created}
      assert secret_created.name == secret.name
      refute secret_created.value == secret.value
    end
  end
end
