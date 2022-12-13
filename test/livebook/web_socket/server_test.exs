defmodule Livebook.WebSocket.ServerTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  @app_version Mix.Project.config()[:version]

  alias Livebook.WebSocket.Server
  alias Livebook.WebSocket.Client.Response

  alias LivebookProto.Request

  describe "connect/2" do
    test "successfully authenticates the websocket connection", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, pid} = Server.start_link()
      assert {:ok, :connected} = Server.connect(pid, url, headers)
      assert Server.disconnect(pid) == :ok
    end

    test "rejects the websocket with invalid address", %{token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, pid} = Server.start_link()

      assert {:error, %Mint.TransportError{reason: :econnrefused}} =
               Server.connect(pid, "http://localhost:9999", headers)

      assert Server.disconnect(pid) == :ok
    end

    test "rejects the websocket connection with invalid credentials", %{url: url} do
      headers = [{"X-Auth-Token", "foo"}]

      assert {:ok, pid} = Server.start_link()
      assert {:error, response} = Server.connect(pid, url, headers)

      assert response.status == 403

      assert %{type: {:error, %{details: error}}} = LivebookProto.Response.decode(response.body)
      assert error =~ "the given token is invalid"

      assert {:error, response} = Server.connect(pid, url)

      assert response.status == 401

      assert %{type: {:error, %{details: error}}} = LivebookProto.Response.decode(response.body)
      assert error =~ "could not get the token from the connection"

      assert Server.disconnect(pid) == :ok
    end
  end

  describe "reconnect event" do
    test "should reconnect after websocket server is up", %{test: name} do
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

      assert {:ok, pid} = Server.start_link()
      assert {:ok, :connected} = Server.connect(pid, url, headers)
      assert Server.connected?(pid)

      Process.monitor(pid)
      leader = Process.group_leader()
      Process.group_leader(pid, leader)

      EnterpriseServer.disconnect(name)
      Process.sleep(500)

      refute Server.connected?(pid)

      # Wait until the server is up again
      assert EnterpriseServer.reconnect(name) == :ok
      Process.sleep(2000)

      assert Server.connected?(pid)
      assert Server.disconnect(pid) == :ok
      refute Server.connected?(pid)

      EnterpriseServer.disconnect(name)
      EnterpriseServer.drop_database(name)
    end
  end

  describe "send_message/2" do
    setup %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      {:ok, pid} = Server.start_link()
      {:ok, :connected} = Server.connect(pid, url, headers)

      on_exit(fn -> Server.disconnect(pid) end)

      {:ok, pid: pid}
    end

    test "successfully sends a session message", %{pid: pid, user: %{id: id, email: email}} do
      session_request = LivebookProto.SessionRequest.new!(app_version: @app_version)
      request = Request.new!(type: {:session, session_request})
      frame = {:binary, Request.encode(request)}

      assert Server.send_message(pid, frame) == :ok

      assert_receive {:ok, %Response{body: body, status: nil, headers: nil}}
      assert {:session, %{id: _, user: %{id: ^id, email: ^email}}} = body
    end
  end
end
