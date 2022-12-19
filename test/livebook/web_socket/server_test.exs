defmodule Livebook.WebSocket.ServerTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  @app_version Mix.Project.config()[:version]
  @moduletag :capture_log

  alias Livebook.WebSocket.Server
  alias Livebook.WebSocket.Client.Response

  setup do
    Livebook.WebSocket.subscribe()

    :ok
  end

  describe "connect" do
    test "successfully authenticates the websocket connection", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, conn} = Server.start_link(url, headers)
      assert_receive {:ok, ^conn, _id, :connected}
      assert Server.connected?(conn)
    end

    test "rejects the websocket with invalid address", %{token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, conn} = Server.start_link("http://localhost:9999", headers)
      refute Server.connected?(conn)
    end

    test "rejects the websocket connection with invalid credentials", %{url: url} do
      headers = [{"X-Auth-Token", "foo"}]

      assert {:ok, conn} = Server.start_link(url, headers)

      assert_receive {:error, ^conn, -1, response}
      assert %Response{body: {:error, %{details: error}}, status: 403} = response

      assert error =~ "the given token is invalid"
      assert Server.close(conn) == :ok

      assert {:ok, conn} = Server.start_link(url)

      assert_receive {:error, ^conn, -1, response}
      assert %Response{body: {:error, %{details: error}}, status: 401} = response

      assert error =~ "could not get the token from the connection"
      assert Server.close(conn) == :ok
    end
  end

  describe "send_request/2" do
    setup %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      {:ok, conn} = Server.start_link(url, headers)

      assert_receive {:ok, ^conn, _id, :connected}

      {:ok, conn: conn}
    end

    test "successfully sends a session request", %{
      conn: conn,
      user: %{id: id, email: email}
    } do
      session_request = LivebookProto.SessionRequest.new!(app_version: @app_version)

      assert Server.send_request(conn, session_request) == :ok
      assert_receive {:ok, ^conn, 0, {:session, response}}
      assert %{id: _, user: %{id: ^id, email: ^email}} = response
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

      assert {:ok, conn} = Server.start_link(url, headers)

      assert_receive {:ok, ^conn, _id, :connected}
      assert Server.connected?(conn)

      on_exit(fn ->
        EnterpriseServer.disconnect(name)
        EnterpriseServer.drop_database(name)
      end)

      {:ok, conn: conn}
    end

    test "receives the disconnect message from websocket server", %{conn: conn, test: name} do
      EnterpriseServer.disconnect(name)

      assert_receive {:error, ^conn, 0, %Response{body: reason}}
      assert %Mint.TransportError{reason: :closed} = reason

      assert Process.alive?(conn)
      refute Server.connected?(conn)
    end

    test "reconnects after websocket server is up", %{conn: conn, test: name} do
      EnterpriseServer.disconnect(name)

      assert_receive {:error, ^conn, 0, %Response{body: reason}}
      assert %Mint.TransportError{reason: :closed} = reason

      Process.sleep(1000)
      refute Server.connected?(conn)

      # Wait until the server is up again
      assert EnterpriseServer.reconnect(name) == :ok
      assert_receive {:ok, ^conn, 0, :connected}, 5000

      assert Server.connected?(conn)
      assert Server.close(conn) == :ok
      refute Server.connected?(conn)
    end
  end
end
