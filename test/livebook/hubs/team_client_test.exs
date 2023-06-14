defmodule Livebook.Hubs.TeamClientTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs.TeamClient

  @moduletag :capture_log

  setup do
    Livebook.Hubs.subscribe([:connection, :secrets])
    :ok
  end

  describe "start_link/1" do
    test "successfully authenticates the web socket connection", %{user: user, node: node} do
      org = :erpc.call(node, Hub.Integration, :create_org, [])
      org_key = :erpc.call(node, Hub.Integration, :create_org_key, [[org: org]])
      org_key_pair = :erpc.call(node, Hub.Integration, :create_org_key_pair, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      team =
        build(:team,
          id: "team-#{org.name}",
          hub_name: org.name,
          user_id: user.id,
          org_id: org.id,
          org_key_id: org_key.id,
          org_public_key: org_key_pair.public_key,
          session_token: token
        )

      id = team.id

      refute TeamClient.connected?(team.id)

      TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
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

      id = team.id

      TeamClient.start_link(team)

      assert_receive {:hub_server_error, ^id, error}

      assert error ==
               "#{team.hub_name}: Your session is out-of-date. Please re-join the organization."

      refute Livebook.Hubs.hub_exists?(team.id)
    end
  end

  describe "handle events" do
    test "receives the secret_created event", %{user: user, node: node} do
      teams_org = build(:org)
      teams_key = teams_org.teams_key
      key_hash = Livebook.Teams.Org.key_hash(teams_org)

      org = :erpc.call(node, Hub.Integration, :create_org, [])

      org_key =
        :erpc.call(node, Hub.Integration, :create_org_key, [[org: org, key_hash: key_hash]])

      org_key_pair = :erpc.call(node, Hub.Integration, :create_org_key_pair, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      team =
        build(:team,
          id: "team-#{org.name}",
          hub_name: org.name,
          user_id: user.id,
          org_id: org.id,
          org_key_id: org_key.id,
          org_public_key: org_key_pair.public_key,
          session_token: token,
          teams_key: teams_key
        )

      id = team.id

      refute TeamClient.connected?(team.id)

      TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}

      secret = build(:secret, name: "FOO", value: "BAR")
      assert Livebook.Teams.create_secret(team, secret) == :ok

      # receives `{:event, :secret_created, secret_created}` event
      # with the value decrypted
      assert_receive {:secret_created, secret_created}, 8000
      assert secret_created.name == secret.name
      assert secret_created.value == secret.value
    end
  end
end
