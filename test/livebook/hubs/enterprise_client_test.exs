defmodule Livebook.Hubs.EnterpriseClientTest do
  use Livebook.EnterpriseIntegrationCase, async: true
  @moduletag :capture_log

  alias Livebook.Hubs.EnterpriseClient
  alias Livebook.Secrets.Secret

  setup do
    Livebook.Hubs.subscribe([:connection, :secrets])
    :ok
  end

  describe "start_link/1" do
    test "successfully authenticates the web socket connection", %{url: url, token: token} do
      enterprise = build(:enterprise, url: url, token: token)

      EnterpriseClient.start_link(enterprise)
      assert_receive :hub_connected
    end

    test "rejects the websocket with invalid address", %{token: token} do
      enterprise = build(:enterprise, url: "http://localhost:9999", token: token)

      EnterpriseClient.start_link(enterprise)
      assert_receive {:hub_connection_failed, "connection refused"}
    end

    test "rejects the web socket connection with invalid credentials", %{url: url} do
      enterprise = build(:enterprise, url: url, token: "foo")

      EnterpriseClient.start_link(enterprise)
      assert_receive {:hub_connection_failed, reason}
      assert reason =~ "the given token is invalid"
    end
  end

  describe "handle events" do
    setup %{test: test, url: url, token: token} do
      node = EnterpriseServer.get_node()
      hub_id = "enterprise-#{test}"

      insert_hub(:enterprise,
        id: hub_id,
        external_id: to_string(test),
        url: url,
        token: token
      )

      assert_receive :hub_connected

      {:ok, node: node, hub_id: hub_id}
    end

    test "receives a secret_created event", %{node: node, hub_id: id} do
      name = "API_TOKEN_ID"
      value = Livebook.Utils.random_id()
      :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])
      secret = %Secret{name: name, value: value, hub_id: id, readonly: true}

      assert_receive {:secret_created, ^secret}
      assert secret in EnterpriseClient.get_secrets(id)
    end

    test "receives a secret_updated event", %{node: node, hub_id: id} do
      name = "SUPER_SUDO_USER"
      value = "JakePeralta"
      new_value = "ChonkyCat"
      enterprise_secret = :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])
      secret = %Secret{name: name, value: value, hub_id: id, readonly: true}
      updated_secret = %Secret{name: name, value: new_value, hub_id: id, readonly: true}

      assert_receive {:secret_created, ^secret}
      assert secret in EnterpriseClient.get_secrets(id)
      refute updated_secret in EnterpriseClient.get_secrets(id)

      :erpc.call(node, Enterprise.Integration, :update_secret, [enterprise_secret, new_value])

      assert_receive {:secret_updated, ^updated_secret}

      assert updated_secret in EnterpriseClient.get_secrets(id)
      refute secret in EnterpriseClient.get_secrets(id)
    end

    test "receives a secret_deleted event", %{node: node, hub_id: id} do
      name = "SUPER_DELETE"
      value = "JakePeralta"
      enteprise_secret = :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])
      secret = %Secret{name: name, value: value, hub_id: id, readonly: true}

      assert_receive {:secret_created, ^secret}
      assert secret in EnterpriseClient.get_secrets(id)

      :erpc.call(node, Enterprise.Integration, :delete_secret, [enteprise_secret])

      assert_receive {:secret_deleted, ^secret}
      refute secret in EnterpriseClient.get_secrets(id)
    end
  end
end
