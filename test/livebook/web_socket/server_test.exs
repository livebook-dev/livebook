defmodule Livebook.WebSocket.ServerTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  @moduletag :capture_log

  alias Livebook.WebSocket.Server

  describe "connect" do
    test "successfully authenticates the websocket connection", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, conn} = Server.start_link(self(), url, headers)
      assert_receive {:connect, :ok, :waiting_upgrade}
      assert_receive {:connect, :ok, :connected}
      assert Server.connected?(conn)
    end

    test "rejects the websocket with invalid address", %{token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, conn} = Server.start_link(self(), "http://localhost:9999", headers)
      refute Server.connected?(conn)
    end

    test "rejects the websocket connection with invalid credentials", %{url: url} do
      headers = [{"X-Auth-Token", "foo"}]

      assert {:ok, conn} = Server.start_link(self(), url, headers)

      assert_receive {:connect, :error, reason}
      assert reason =~ "the given token is invalid"
      assert Server.close(conn) == :ok

      assert {:ok, conn} = Server.start_link(self(), url)

      assert_receive {:connect, :error, reason}
      assert reason =~ "could not get the token from the connection"
      assert Server.close(conn) == :ok
    end
  end

  describe "send_request/2" do
    setup %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      {:ok, conn} = Server.start_link(self(), url, headers)

      assert_receive {:connect, :ok, :waiting_upgrade}
      assert_receive {:connect, :ok, :connected}

      {:ok, conn: conn}
    end

    test "successfully sends a session request", %{
      conn: conn,
      user: %{id: id, email: email}
    } do
      session_request =
        LivebookProto.SessionRequest.new!(app_version: Livebook.Config.app_version())

      assert {:session, session_response} = Server.send_request(conn, session_request)
      assert %{id: _, user: %{id: ^id, email: ^email}} = session_response
    end
  end

  describe "reconnect event" do
    setup %{test: name} do
      suffix = Ecto.UUID.generate() |> :erlang.phash2() |> to_string()
      app_port = Enum.random(1000..9000) |> to_string()

      {:ok, _} =
        EnterpriseServer.start(name,
          env: %{"ENTERPRISE_DB_SUFFIX" => suffix},
          app_port: app_port
        )

      url = EnterpriseServer.url(name)
      token = EnterpriseServer.token(name)
      headers = [{"X-Auth-Token", token}]

      assert {:ok, conn} = Server.start_link(self(), url, headers)

      assert_receive {:connect, :ok, :waiting_upgrade}
      assert_receive {:connect, :ok, :connected}
      assert Server.connected?(conn)

      on_exit(fn ->
        EnterpriseServer.disconnect(name)
        EnterpriseServer.drop_database(name)
      end)

      {:ok, conn: conn}
    end

    test "receives the disconnect message from websocket server", %{conn: conn, test: name} do
      EnterpriseServer.disconnect(name)

      assert_receive {:connect, :error, %Mint.TransportError{reason: :closed}}
      assert_receive {:connect, :error, %Mint.TransportError{reason: :econnrefused}}

      assert Process.alive?(conn)
      refute Server.connected?(conn)
    end

    test "reconnects after websocket server is up", %{conn: conn, test: name} do
      EnterpriseServer.disconnect(name)

      assert_receive {:connect, :error, %Mint.TransportError{reason: :closed}}
      assert_receive {:connect, :error, %Mint.TransportError{reason: :econnrefused}}

      Process.sleep(1000)
      refute Server.connected?(conn)

      # Wait until the server is up again
      assert EnterpriseServer.reconnect(name) == :ok

      assert_receive {:connect, :ok, :waiting_upgrade}, 3000
      assert_receive {:connect, :ok, :connected}, 3000

      assert Server.connected?(conn)
      assert Server.close(conn) == :ok
      refute Server.connected?(conn)
    end
  end
end
