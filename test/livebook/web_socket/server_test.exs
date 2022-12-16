defmodule Livebook.WebSocket.ServerTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  @app_version Mix.Project.config()[:version]

  alias Livebook.WebSocket.Server
  alias Livebook.WebSocket.Client.Response

  alias LivebookProto.Request

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

      assert_receive {:error, ^conn, _id,
                      %Response{body: {:error, %{details: error}}, status: 403}}

      assert error =~ "the given token is invalid"
      assert Server.close(conn) == :ok

      assert {:ok, conn} = Server.start_link(url)

      assert_receive {:error, ^conn, _id,
                      %Response{body: {:error, %{details: error}}, status: 401}}

      assert error =~ "could not get the token from the connection"
      assert Server.close(conn) == :ok
    end
  end

  describe "send_message/2" do
    setup %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      {:ok, conn} = Server.start_link(url, headers)

      assert_receive {:ok, ^conn, _id, :connected}

      {:ok, conn: conn}
    end

    test "successfully sends a session message", %{
      conn: conn,
      user: %{id: user_id, email: email}
    } do
      session_request = LivebookProto.SessionRequest.new!(app_version: @app_version)
      id = Livebook.Utils.random_id()
      request = Request.new!(id: id, type: {:session, session_request})
      frame = {:binary, Request.encode(request)}

      assert Server.send_message(conn, frame, request.id) == :ok
      assert_receive {:ok, ^conn, ^id, %Response{body: {:session, response}}}
      assert %{id: _, user: %{id: ^user_id, email: ^email}} = response
    end
  end

  describe "reconnect event" do
    @describetag :capture_log

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

      assert_receive {:error, ^conn, _id, %Response{body: reason}}
      assert %Mint.TransportError{reason: :closed} = reason

      assert Process.alive?(conn)
      refute Server.connected?(conn)
    end

    test "should reconnect after websocket server is up", %{conn: conn, test: name} do
      EnterpriseServer.disconnect(name)

      assert_receive {:error, ^conn, _id, %Response{body: reason}}
      assert %Mint.TransportError{reason: :closed} = reason

      Process.sleep(1000)
      refute Server.connected?(conn)

      # Wait until the server is up again
      assert EnterpriseServer.reconnect(name) == :ok
      assert_receive {:ok, ^conn, _id, :connected}, 5000

      assert Server.connected?(conn)
      assert Server.close(conn) == :ok
      assert_receive {:ok, ^conn, _id, :disconnected}
      refute Server.connected?(conn)
    end
  end
end
