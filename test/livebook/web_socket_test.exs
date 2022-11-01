defmodule Livebook.WebSocketTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  @app_version Mix.Project.config()[:version]

  alias Livebook.WebSocket

  describe "authentication" do
    test "successfully authenticates the web socket connection", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, connection, :connected} = WebSocket.connect(url, headers)
      assert WebSocket.disconnect(connection) == :ok
    end

    test "rejects the web socket connection with invalid credentials", %{url: url} do
      headers = [{"X-Auth-Token", "foo"}]

      assert {:error, connection, %{details: "Invalid Token"}} = WebSocket.connect(url, headers)
      assert WebSocket.disconnect(connection) == :ok

      assert {:error, connection, %{details: "Token not found"}} = WebSocket.connect(url)
      assert WebSocket.disconnect(connection) == :ok
    end
  end

  describe "send_request/2" do
    test "receives the session response from server", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, %WebSocket.Connection{} = connection, :connected} =
               WebSocket.connect(url, headers)

      session_request = LivebookProto.SessionRequest.new!(app_version: @app_version)

      assert {:ok, %WebSocket.Connection{} = connection} =
               WebSocket.send_request(connection, session_request)

      assert {:ok, connection, {:session, session_response}} =
               WebSocket.receive_response(connection)

      assert WebSocket.disconnect(connection) == :ok

      assert session_response.user.email == "jake.peralta@mail.com"
    end
  end
end
