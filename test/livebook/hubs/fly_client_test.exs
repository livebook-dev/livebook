defmodule Livebook.Hubs.FlyClientTest do
  use ExUnit.Case

  alias Livebook.Hubs.{Fly, FlyClient}

  setup do
    bypass = Bypass.open()

    Application.put_env(
      :livebook,
      :fly_graphql_endpoint,
      "http://localhost:#{bypass.port}"
    )

    on_exit(fn ->
      Application.delete_env(:livebook, :fly_graphql_endpoint)
    end)

    {:ok, bypass: bypass, url: "http://localhost:#{bypass.port}"}
  end

  describe "fetch_apps/1" do
    test "fetches an empty list of apps", %{bypass: bypass} do
      response = %{"data" => %{"apps" => %{"nodes" => []}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, []} = FlyClient.fetch_apps("some valid token")
    end

    test "fetches a list of apps", %{bypass: bypass} do
      app = %{
        "id" => "foo-app",
        "organization" => %{
          "id" => "l3soyvjmvtmwtl6l2drnbfuvltipprge",
          "name" => "Foo Bar",
          "type" => "PERSONAL"
        }
      }

      app_id = app["id"]

      response = %{"data" => %{"apps" => %{"nodes" => [app]}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, [%Fly{application_id: ^app_id}]} = FlyClient.fetch_apps("some valid token")
    end

    test "returns unauthorized when token is invalid", %{bypass: bypass} do
      error = %{"extensions" => %{"code" => "UNAUTHORIZED"}}
      response = %{"data" => nil, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:error, "request failed with code: UNAUTHORIZED"} = FlyClient.fetch_apps("foo")
    end
  end
end
