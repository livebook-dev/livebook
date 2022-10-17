defmodule Livebook.WebSocketTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  alias Livebook.WebSocket

  describe "authentication" do
    test "successfully authenticates the web socket connection", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, socket} = WebSocket.connect(url, headers)
      assert is_pid(socket)
    end

    test "rejects the web socket connection with invalid credentials", %{url: url} do
      headers = [{"X-Auth-Token", "foo"}]

      assert {:error, "Invalid Token"} = WebSocket.connect(url, headers)
      assert {:error, "Token not found"} = WebSocket.connect(url)
    end
  end

  describe "send_session/1" do
    test "receives the session response from server", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, socket} = WebSocket.connect(url, headers)
      assert is_pid(socket)

      assert :ok = WebSocket.send_session(socket)

      assert_receive {:session, %{id: _, user: _}}
    end
  end
end
