defmodule Livebook.Hubs.EnterpriseClientTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  alias Livebook.Hubs.EnterpriseClient

  describe "fetch_info/1" do
    test "fetches the token info", %{url: url, token: token} do
      assert {:ok, %{"id" => _}} = EnterpriseClient.fetch_info(url, token)
    end

    test "returns invalid_token when token is invalid", %{url: url} do
      assert {:error, _, :invalid_token} = EnterpriseClient.fetch_info(url, "foo")
    end
  end

  describe "fetch_me/1" do
    test "fetches the current user id", %{url: url, token: token} do
      assert {:ok, %{"id" => _}} = EnterpriseClient.fetch_me(url, token)
    end

    test "returns unauthorized when token is invalid", %{url: url} do
      assert {:error, _, :unauthorized} = EnterpriseClient.fetch_me(url, "foo")
    end
  end
end
