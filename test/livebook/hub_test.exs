defmodule Livebook.HubsTest do
  use ExUnit.Case

  alias Livebook.Hubs
  alias Livebook.Hubs.Hub

  describe "Fly fetch_hubs/1" do
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

      {:ok, bypass: bypass}
    end

    test "fetches an empty list of hubs", %{bypass: bypass} do
      response = %{"data" => %{"apps" => %{"nodes" => []}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, []} = Hubs.fetch_hubs(%Fly{token: "some valid token"})
    end

    test "fetches a list of hubs", %{bypass: bypass} do
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

      assert {:ok, [hub]} = Hubs.fetch_hubs(%Fly{token: "some valid token"})

      assert hub == %Hub{
               id: "l3soyvjmvtmwtl6l2drnbfuvltipprge",
               type: "fly",
               label: "Foo Bar",
               token: "some valid token"
             }
    end
  end
end
