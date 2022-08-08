defmodule Livebook.Hub.FlyTest do
  use ExUnit.Case

  alias Livebook.Hub.Fly

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

    {:ok, bypass: bypass}
  end

  describe "fetch_applications/1" do
    test "fetches an empty list of applications", %{bypass: bypass} do
      response = %{"data" => %{"apps" => %{"nodes" => []}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, []} = Fly.fetch_applications("some valid token")
    end

    test "fetches a list of applications", %{bypass: bypass} do
      app = %{
        "id" => "my-foo-application",
        "name" => "my-foo-application",
        "deployed" => true,
        "hostname" => "https://my-foo-application.fly.dev",
        "organization" => %{
          "id" => "l3soyvjmvtmwtl6l2drnbfuvltipprge",
          "slug" => "personal",
          "name" => "Foo Bar",
          "type" => "PERSONAL"
        },
        "latestImageDetails" => %{
          "registry" => "registry-1.docker.io",
          "repository" => "livebook/livebook",
          "tag" => "0.6.3"
        },
        "state" => "DEPLOYED"
      }

      response = %{"data" => %{"apps" => %{"nodes" => [app]}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, [application]} = Fly.fetch_applications("some valid token")

      assert application == %Fly{
               deployed: true,
               hostname: "https://my-foo-application.fly.dev",
               id: "my-foo-application",
               image_details: %{
                 registry: "registry-1.docker.io",
                 repository: "livebook/livebook",
                 tag: "0.6.3"
               },
               name: "my-foo-application",
               organization: %{
                 id: "l3soyvjmvtmwtl6l2drnbfuvltipprge",
                 name: "Foo Bar",
                 slug: "personal",
                 type: "PERSONAL"
               },
               state: "DEPLOYED",
               token: "some valid token"
             }
    end

    test "returns unauthorized when token is invalid", %{bypass: bypass} do
      error = %{"extensions" => %{"code" => "UNAUTHORIZED"}}
      response = %{"data" => nil, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:error, :unauthorized} = Fly.fetch_applications("foo")
    end
  end
end
