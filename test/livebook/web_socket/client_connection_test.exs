defmodule Livebook.WebSocket.ClientConnectionTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  @moduletag :capture_log

  alias Livebook.WebSocket.ClientConnection

  describe "connect" do
    test "successfully authenticates the websocket connection", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, _conn} = ClientConnection.start_link(self(), url, headers)
      assert_receive {:connect, :ok, :connected}
    end

    test "rejects the websocket with invalid address", %{token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, _conn} = ClientConnection.start_link(self(), "http://localhost:9999", headers)
      assert_receive {:connect, :error, "connection refused"}
    end

    test "rejects the websocket connection with invalid credentials", %{url: url} do
      headers = [{"X-Auth-Token", "foo"}]

      assert {:ok, _conn} = ClientConnection.start_link(self(), url, headers)

      assert_receive {:connect, :error, reason}
      assert reason =~ "the given token is invalid"

      assert {:ok, _conn} = ClientConnection.start_link(self(), url)

      assert_receive {:connect, :error, reason}
      assert reason =~ "could not get the token from the connection"
    end
  end

  describe "send_request/2" do
    setup %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      {:ok, conn} = ClientConnection.start_link(self(), url, headers)
      assert_receive {:connect, :ok, :connected}

      {:ok, conn: conn}
    end

    test "successfully sends a session request", %{conn: conn, user: %{id: id, email: email}} do
      data = LivebookProto.build_handshake_request(app_version: Livebook.Config.app_version())

      assert {:handshake, handshake_response} = ClientConnection.send_request(conn, data)
      assert %{id: _, user: %{id: ^id, email: ^email}} = handshake_response
    end

    test "successfully sends a create secret message", %{conn: conn} do
      data = LivebookProto.build_create_secret_request(name: "MY_USERNAME", value: "Jake Peralta")
      assert {:create_secret, _} = ClientConnection.send_request(conn, data)
    end

    test "sends a create secret message, but receive a changeset error", %{conn: conn} do
      data = LivebookProto.build_create_secret_request(name: "MY_USERNAME", value: "")
      assert {:changeset_error, errors} = ClientConnection.send_request(conn, data)
      assert "can't be blank" in errors.value
    end
  end

  describe "reconnect event" do
    setup %{test: name} do
      start_new_instance(name)

      url = EnterpriseServer.url(name)
      token = EnterpriseServer.token(name)
      headers = [{"X-Auth-Token", token}]

      assert {:ok, conn} = ClientConnection.start_link(self(), url, headers)
      assert_receive {:connect, :ok, :connected}

      on_exit(fn -> stop_new_instance(name) end)

      {:ok, conn: conn}
    end

    test "receives the disconnect message from websocket server", %{conn: conn, test: name} do
      EnterpriseServer.disconnect(name)

      assert_receive {:connect, :error, "socket closed"}
      assert_receive {:connect, :error, "connection refused"}

      assert Process.alive?(conn)
    end

    test "reconnects after websocket server is up", %{test: name} do
      EnterpriseServer.disconnect(name)

      assert_receive {:connect, :error, "socket closed"}
      assert_receive {:connect, :error, "connection refused"}

      Process.sleep(1000)

      # Wait until the server is up again
      assert EnterpriseServer.reconnect(name) == :ok

      assert_receive {:connect, :ok, :connected}, 5000
    end
  end

  describe "handle events from server" do
    setup %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      {:ok, conn} = ClientConnection.start_link(self(), url, headers)
      assert_receive {:connect, :ok, :connected}

      {:ok, conn: conn}
    end

    test "receives a secret_created event", %{node: node} do
      name = "MY_SECRET_ID"
      value = Livebook.Utils.random_id()
      :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])

      assert_receive {:event, :secret_created, %{name: ^name, value: ^value}}
    end

    test "receives a secret_updated event", %{node: node} do
      name = "API_USERNAME"
      value = "JakePeralta"
      secret = :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])

      assert_receive {:event, :secret_created, %{name: ^name, value: ^value}}

      new_value = "ChonkyCat"
      :erpc.call(node, Enterprise.Integration, :update_secret, [secret, new_value])

      assert_receive {:event, :secret_updated, %{name: ^name, value: ^new_value}}
    end

    test "receives a secret_deleted event", %{node: node} do
      name = "DELETE_ME"
      value = "JakePeralta"
      secret = :erpc.call(node, Enterprise.Integration, :create_secret, [name, value])

      assert_receive {:event, :secret_created, %{name: ^name, value: ^value}}

      :erpc.call(node, Enterprise.Integration, :delete_secret, [secret])

      assert_receive {:event, :secret_deleted, %{name: ^name, value: ^value}}
    end

    test "receives a user_synchronized event", %{conn: conn, node: node} do
      data = LivebookProto.build_handshake_request(app_version: Livebook.Config.app_version())
      assert {:handshake, _} = ClientConnection.send_request(conn, data)

      id = :erpc.call(node, Enterprise.Integration, :fetch_env!, ["ENTERPRISE_ID"])
      name = :erpc.call(node, Enterprise.Integration, :fetch_env!, ["ENTERPRISE_NAME"])

      assert_receive {:event, :user_synchronized, %{id: ^id, name: ^name, secrets: []}}

      secret = :erpc.call(node, Enterprise.Integration, :create_secret, ["SESSION", "123"])
      assert_receive {:event, :secret_created, %{name: "SESSION", value: "123"}}

      assert {:handshake, _} = ClientConnection.send_request(conn, data)

      assert_receive {:event, :user_synchronized, %{id: ^id, name: ^name, secrets: secrets}}
      assert LivebookProto.Secret.new!(name: secret.name, value: secret.value) in secrets
    end
  end
end
