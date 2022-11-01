defmodule Livebook.WebSocket.ClientTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  alias Livebook.WebSocket.Client

  describe "connect/2" do
    test "successfully authenticates the websocket connection", %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:ok, conn, ref} = Client.connect(url, headers)
      assert {:ok, conn, websocket, :connected} = Client.receive(conn, ref)
      assert Client.disconnect(conn, websocket, ref) == :ok
    end

    test "rejects the websocket with invalid address", %{token: token} do
      headers = [{"X-Auth-Token", token}]

      assert {:error, %Mint.TransportError{reason: :econnrefused}} =
               Client.connect("http://localhost:9999", headers)
    end

    test "rejects the websocket connection with invalid credentials", %{url: url} do
      headers = [{"X-Auth-Token", "foo"}]

      assert {:ok, conn, ref} = Client.connect(url, headers)
      assert {:error, _conn, response} = Client.receive(conn, ref)

      assert response.status == 403
      assert %{type: {:error, %{details: "Invalid Token"}}} = response.body

      assert {:ok, conn, ref} = Client.connect(url)
      assert {:error, _conn, response} = Client.receive(conn, ref)

      assert response.status == 401
      assert %{type: {:error, %{details: "Token not found"}}} = response.body
    end
  end
end
