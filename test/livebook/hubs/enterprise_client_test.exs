defmodule Livebook.Hubs.EnterpriseClientTest do
  use Livebook.EnterpriseIntegrationCase, async: true
  @moduletag :capture_log

  alias Livebook.Hubs.EnterpriseClient
  alias Livebook.Secrets.Secret

  setup do
    EnterpriseClient.subscribe()
    :ok
  end

  describe "start_link/1" do
    test "successfully authenticates the web socket connection", %{url: url, token: token} do
      enterprise = build(:enterprise, url: url, token: token)

      EnterpriseClient.start_link(enterprise)
      assert_receive {:connect, :ok, :connected}
    end

    test "rejects the websocket with invalid address", %{token: token} do
      enterprise = build(:enterprise, url: "http://localhost:9999", token: token)

      EnterpriseClient.start_link(enterprise)
      assert_receive {:connect, :error, "connection refused"}
    end

    test "rejects the web socket connection with invalid credentials", %{url: url} do
      enterprise = build(:enterprise, url: url, token: "foo")

      EnterpriseClient.start_link(enterprise)
      assert_receive {:connect, :error, reason}
      assert reason =~ "the given token is invalid"
    end
  end

  describe "handle events" do
    setup %{url: url, token: token} do
      enterprise = build(:enterprise, url: url, token: token)
      EnterpriseClient.start_link(enterprise)

      assert_receive {:connect, :ok, :connected}

      :ok
    end

    test "receives a secret_created event" do
      name = "API_TOKEN_ID"
      value = Livebook.Utils.random_id()
      node = EnterpriseServer.get_node()
      :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])

      assert_receive {:secret_created, %Secret{name: ^name, value: ^value}}
    end

    test "receives a secret_updated event" do
      name = "SUPER_SUDO_USER"
      value = "JakePeralta"
      node = EnterpriseServer.get_node()
      secret = :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])

      assert_receive {:secret_created, %Secret{name: ^name, value: ^value}}

      new_value = "ChonkyCat"
      :erpc.call(node, Enterprise.Integration, :update_secret, [secret, new_value])

      assert_receive {:secret_updated, %Secret{name: ^name, value: ^new_value}}
    end
  end
end
