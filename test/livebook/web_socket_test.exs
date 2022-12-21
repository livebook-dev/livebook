defmodule Livebook.WebSocketTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  @app_version Mix.Project.config()[:version]

  alias Livebook.WebSocket

  describe "authentication" do
    test "successfully authenticates the web socket connection", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, connection, :connected} = WebSocket.connect(url, headers)
      assert {:ok, _connection} = WebSocket.disconnect(connection)
    end

    test "rejects the web socket connection with invalid credentials", %{url: url} do
      headers = [{"X-Auth-Token", "foo"}]

      assert {:error, connection, %{details: error}} = WebSocket.connect(url, headers)
      assert error =~ "the given token is invalid"
      assert {:ok, _connection} = WebSocket.disconnect(connection)

      assert {:error, connection, %{details: error}} = WebSocket.connect(url)
      assert error =~ "could not get the token from the connection"
      assert {:ok, _connection} = WebSocket.disconnect(connection)
    end
  end

  describe "send_request/2" do
    setup %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      {:ok, %WebSocket.Connection{} = connection, :connected} = WebSocket.connect(url, headers)

      on_exit(fn -> WebSocket.disconnect(connection) end)

      {:ok, connection: connection}
    end

    test "successfully sends a session message", %{
      connection: connection,
      user: %{id: id, email: email}
    } do
      session_request = LivebookProto.SessionRequest.new!(app_version: @app_version)

      assert {:ok, %WebSocket.Connection{} = connection} =
               WebSocket.send_request(connection, session_request)

      assert {:ok, ^connection, response} = WebSocket.receive_response(connection)
      assert {:session, %{id: _, user: %{id: ^id, email: ^email}}} = response
    end
  end
end
