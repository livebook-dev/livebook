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

      assert_receive {:secret_created, %Secret{name: ^name, value: ^value, origin: ^id}}
    end

    test "receives a secret_updated event", %{node: node, hub_id: id} do
      name = "SUPER_SUDO_USER"
      value = "JakePeralta"
      secret = :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])

      assert_receive {:secret_created, %Secret{name: ^name, value: ^value, origin: ^id}}

      new_value = "ChonkyCat"
      :erpc.call(node, Enterprise.Integration, :update_secret, [secret, new_value])

      assert_receive {:secret_updated, %Secret{name: ^name, value: ^new_value, origin: ^id}}
    end
  end
end
