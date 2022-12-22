defmodule Livebook.Hubs.EnterpriseClientTest do
  use Livebook.EnterpriseIntegrationCase, async: true
  @moduletag :capture_log

  alias Livebook.Hubs.EnterpriseClient

  describe "start_link/1" do
    test "successfully authenticates the web socket connection", %{url: url, token: token} do
      enterprise = build(:enterprise, url: url, token: token)

      assert {:ok, _pid} = EnterpriseClient.start_link(enterprise)
    end

    test "rejects the websocket with invalid address", %{token: token} do
      enterprise = build(:enterprise, url: "http://localhost:9999", token: token)
      Livebook.WebSocket.subscribe()

      assert {:ok, _pid} = EnterpriseClient.start_link(enterprise)
      assert_receive {:connect, :error, %Mint.TransportError{reason: :econnrefused}}
    end

    test "rejects the web socket connection with invalid credentials", %{url: url} do
      enterprise = build(:enterprise, url: url, token: "foo")
      Livebook.WebSocket.subscribe()

      assert {:ok, _pid} = EnterpriseClient.start_link(enterprise)
      assert_receive {:connect, :error, reason}
      assert reason =~ "the given token is invalid"
    end
  end

  describe "send_request/1" do
    setup %{url: url, token: token} do
      enterprise = build(:enterprise, url: url, token: token)
      Livebook.WebSocket.subscribe()

      {:ok, pid} = EnterpriseClient.start_link(enterprise)
      refute_receive {:connect, :error, _}

      on_exit(fn -> EnterpriseClient.disconnect(pid) end)

      {:ok, pid: pid}
    end

    test "successfully sends a session message", %{pid: pid, user: %{id: id, email: email}} do
      session_request =
        LivebookProto.SessionRequest.new!(app_version: Livebook.Config.app_version())

      assert {:session, %{id: _, user: %{id: ^id, email: ^email}}} =
               EnterpriseClient.send_request(pid, session_request)
    end
  end
end
