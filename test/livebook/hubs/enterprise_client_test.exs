defmodule Livebook.Hubs.EnterpriseClientTest do
  use Livebook.EnterpriseIntegrationCase, async: true
  @moduletag :capture_log

  alias Livebook.Hubs.EnterpriseClient

  setup do
    EnterpriseClient.subscribe()
    :ok
  end

  describe "start_link/1" do
    test "successfully authenticates the web socket connection", %{url: url, token: token} do
      enterprise = build(:enterprise, url: url, token: token)

      assert {:ok, _pid} = EnterpriseClient.start_link(enterprise)
      assert_receive {:connect, :ok, :waiting_upgrade}
      assert_receive {:connect, :ok, :connected}
    end

    test "rejects the websocket with invalid address", %{token: token} do
      enterprise = build(:enterprise, url: "http://localhost:9999", token: token)

      assert {:ok, _pid} = EnterpriseClient.start_link(enterprise)
      assert_receive {:connect, :error, %Mint.TransportError{reason: :econnrefused}}
    end

    test "rejects the web socket connection with invalid credentials", %{url: url} do
      enterprise = build(:enterprise, url: url, token: "foo")

      assert {:ok, _pid} = EnterpriseClient.start_link(enterprise)
      assert_receive {:connect, :error, reason}
      assert reason =~ "the given token is invalid"
    end
  end
end
