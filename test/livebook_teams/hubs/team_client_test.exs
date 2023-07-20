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
      team = create_team_hub(user, node)
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
      team = create_team_hub(user, node)
      id = team.id

      refute TeamClient.connected?(team.id)

      TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}

      secret = build(:secret, name: "SECRET_CREATED_FOO", value: "BAR")
      assert Livebook.Teams.create_secret(team, secret) == :ok

      name = secret.name
      value = secret.value

      # receives `{:event, :secret_created, secret_created}` event
      # with the value decrypted
      assert_receive {:secret_created, %{name: ^name, value: ^value}}
    end

    test "receives the secret_updated event", %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      refute TeamClient.connected?(team.id)

      TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}

      secret = build(:secret, name: "SECRET_UPDATED_FOO", value: "BAR")
      assert Livebook.Teams.create_secret(team, secret) == :ok

      name = secret.name
      value = secret.value

      # receives `{:secret_created, secret_created}` event
      assert_receive {:secret_created, %{name: ^name, value: ^value}}

      # updates the secret
      update_secret = Map.replace!(secret, :value, "BAZ")
      assert Livebook.Teams.update_secret(team, update_secret) == :ok

      new_value = update_secret.value

      # receives `{:secret_updated, secret_updated}` event
      # with the value decrypted
      assert_receive {:secret_updated, %{name: ^name, value: ^new_value}}
    end

    test "receives the secret_deleted event", %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      refute TeamClient.connected?(team.id)

      TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}

      secret = build(:secret, name: "SECRET_DELETED_FOO", value: "BAR")
      assert Livebook.Teams.create_secret(team, secret) == :ok

      name = secret.name
      value = secret.value

      # receives `{:secret_created, secret_created}` event
      assert_receive {:secret_created, %{name: ^name, value: ^value}}

      # deletes the secret
      assert Livebook.Teams.delete_secret(team, secret) == :ok

      # receives `{:secret_deleted, secret_deleted}` event
      assert_receive {:secret_deleted, %{name: ^name, value: ^value}}
    end
  end
end
