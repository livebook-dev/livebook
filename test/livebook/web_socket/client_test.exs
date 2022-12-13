defmodule Livebook.WebSocket.ClientTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  @app_version Mix.Project.config()[:version]

  alias Livebook.WebSocket.Client
  alias LivebookProto.Request

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

      assert %{type: {:error, %{details: error}}} = LivebookProto.Response.decode(response.body)
      assert error =~ "the given token is invalid"

      assert {:ok, conn, ref} = Client.connect(url)
      assert {:error, _conn, response} = Client.receive(conn, ref)

      assert response.status == 401

      assert %{type: {:error, %{details: error}}} = LivebookProto.Response.decode(response.body)
      assert error =~ "could not get the token from the connection"
    end
  end

  describe "send/2" do
    setup %{url: url, token: token} do
      headers = [{"X-Auth-Token", token}]

      {:ok, conn, ref} = Client.connect(url, headers)
      {:ok, conn, websocket, :connected} = Client.receive(conn, ref)

      on_exit(fn -> Client.disconnect(conn, websocket, ref) end)

      {:ok, conn: conn, websocket: websocket, ref: ref}
    end

    test "successfully sends a session message", %{
      conn: conn,
      websocket: websocket,
      ref: ref,
      user: %{id: id, email: email}
    } do
      session_request = LivebookProto.SessionRequest.new!(app_version: @app_version)
      request = Request.new!(type: {:session, session_request})
      frame = {:binary, Request.encode(request)}

      assert {:ok, conn, websocket} = Client.send(conn, websocket, ref, frame)

      assert {:ok, ^conn, ^websocket, %Client.Response{body: body}} =
               Client.receive(conn, ref, websocket)

      assert %{type: result} = LivebookProto.Response.decode(body)
      assert {:session, %{id: _, user: %{id: ^id, email: ^email}}} = result
    end
  end
end
