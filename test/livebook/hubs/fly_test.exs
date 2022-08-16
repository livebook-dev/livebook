defmodule Livebook.Hubs.FlyTest do
  use ExUnit.Case

  alias Livebook.Hubs.Fly

  setup do
    bypass = Bypass.open()

    Application.put_env(
      :livebook,
      :fly_io_graphql_endpoint,
      "http://localhost:#{bypass.port}"
    )

    on_exit(fn ->
      Application.delete_env(:livebook, :fly_io_graphql_endpoint)
    end)

    {:ok, bypass: bypass, url: "http://localhost:#{bypass.port}"}
  end

  describe "fetch_organizations/1" do
    test "fetches an empty list of organizations", %{bypass: bypass} do
      response = %{"data" => %{"organizations" => %{"nodes" => []}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, []} = Fly.fetch_organizations("some valid token")
    end

    test "fetches a list of organizations", %{bypass: bypass} do
      organization = %{
        "id" => "l3soyvjmvtmwtl6l2drnbfuvltipprge",
        "slug" => "personal",
        "name" => "Foo Bar",
        "type" => "PERSONAL"
      }

      org_id = organization["id"]

      response = %{"data" => %{"organizations" => %{"nodes" => [organization]}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, [%Fly.Organization{id: ^org_id}]} = Fly.fetch_organizations("some valid token")
    end

    test "returns unauthorized when token is invalid", %{bypass: bypass} do
      error = %{"extensions" => %{"code" => "UNAUTHORIZED"}}
      response = %{"data" => nil, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:error, "request failed with code: UNAUTHORIZED"} = Fly.fetch_organizations("foo")
    end
  end
end
