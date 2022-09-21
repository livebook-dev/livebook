defmodule Livebook.Hubs.EnterpriseClientTest do
  use Livebook.DataCase

  alias Livebook.Hubs.EnterpriseClient

  setup do
    bypass = Bypass.open()
    {:ok, bypass: bypass, url: "http://localhost:#{bypass.port}"}
  end

  describe "fetch_info/1" do
    test "fetches the token info", %{bypass: bypass, url: url} do
      info = %{
        "id" => Livebook.Utils.random_short_id(),
        "expire_at" => to_string(DateTime.utc_now())
      }

      response = %{"data" => %{"info" => info}}

      Bypass.expect_once(bypass, "POST", "/api/v1", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert EnterpriseClient.fetch_info(%{"url" => url, "token" => "some valid token"}) ==
               {:ok, info}
    end

    test "returns invalid_token when token is invalid", %{bypass: bypass, url: url} do
      error = %{"message" => "invalid_token"}
      response = %{"data" => %{"info" => nil}, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/api/v1", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert EnterpriseClient.fetch_info(%{"url" => url, "token" => "foo"}) ==
               {:error, "invalid_token"}
    end
  end

  describe "fetch_me/1" do
    test "fetches the current user id", %{bypass: bypass, url: url} do
      me = %{"id" => Livebook.Utils.random_short_id()}
      response = %{"data" => %{"me" => me}}

      Bypass.expect_once(bypass, "POST", "/api/v1", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert EnterpriseClient.fetch_me(%{"url" => url, "token" => "some valid token"}) ==
               {:ok, me}
    end

    test "returns unauthorized when token is invalid", %{bypass: bypass, url: url} do
      error = %{"message" => "unauthorized"}
      response = %{"data" => %{"me" => nil}, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/api/v1", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert EnterpriseClient.fetch_info(%{"url" => url, "token" => "foo"}) ==
               {:error, "unauthorized"}
    end
  end
end
