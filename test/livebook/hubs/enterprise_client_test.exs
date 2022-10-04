defmodule Livebook.Hubs.EnterpriseClientTest do
  use Livebook.IntegrationCase

  alias Livebook.Hubs.EnterpriseClient

  describe "fetch_info/1" do
    test "fetches the token info" do
      url = LivebookTest.Enterprise.url()
      token = LivebookTest.Enterprise.fetch_token()

      assert {:ok, %{"id" => _, "expire_at" => _}} =
               EnterpriseClient.fetch_info(%{"url" => url, "token" => token})
    end

    test "returns invalid_token when token is invalid" do
      url = LivebookTest.Enterprise.url()

      assert {:error, _, :invalid_token} =
               EnterpriseClient.fetch_info(%{"url" => url, "token" => "foo"})
    end
  end

  describe "fetch_me/1" do
    test "fetches the current user id" do
      url = LivebookTest.Enterprise.url()
      token = LivebookTest.Enterprise.fetch_token()

      assert {:ok, _} = EnterpriseClient.fetch_me(%{"url" => url, "token" => token})
    end

    test "returns unauthorized when token is invalid" do
      url = LivebookTest.Enterprise.url()

      assert {:error, _, :unauthorized} =
               EnterpriseClient.fetch_me(%{"url" => url, "token" => "foo"})
    end
  end
end
