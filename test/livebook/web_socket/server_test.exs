defmodule Livebook.WebSocket.ServerTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  alias Livebook.WebSocket.Server

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
end
