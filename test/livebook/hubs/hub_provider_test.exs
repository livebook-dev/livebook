defmodule Livebook.Hubs.HubProviderTest do
  use ExUnit.Case

  alias Livebook.Hubs.HubProvider
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
      response = %{"data" => %{"organizations" => %{"nodes" => []}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, []} = HubProvider.fetch_hubs(%Fly{token: "some valid token"})
    end

    test "fetches a list of hubs", %{bypass: bypass} do
      organization = %{
        "id" => "l3soyvjmvtmwtl6l2drnbfuvltipprge",
        "slug" => "personal",
        "name" => "Foo Bar",
        "type" => "PERSONAL"
      }

      response = %{"data" => %{"organizations" => %{"nodes" => [organization]}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      assert {:ok, [hub]} = HubProvider.fetch_hubs(%Fly{token: "some valid token"})

      assert hub == %Hub{
               id: "l3soyvjmvtmwtl6l2drnbfuvltipprge",
               type: "fly",
               label: "Foo Bar",
               token: "some valid token"
             }
    end
  end
end
