defmodule Livebook.Hubs.FlyClientTest do
  use Livebook.DataCase

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

  describe "fetch_app/1" do
    test "fetches an application", %{bypass: bypass} do
      app = %{
        "id" => "foo-app",
        "name" => "foo-app",
        "hostname" => "foo-app.fly.dev",
        "platformVersion" => "nomad",
        "deployed" => true,
        "status" => "running",
        "secrets" => [
          %{
            "createdAt" => to_string(DateTime.utc_now()),
            "digest" => to_string(Livebook.Utils.random_cookie()),
            "id" => Livebook.Utils.random_short_id(),
            "name" => "FOO"
          }
        ]
      }

      response = %{"data" => %{"app" => app}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      hub = build(:fly)
      assert {:ok, ^app} = FlyClient.fetch_app(hub)
    end

    test "returns unauthorized when token is invalid", %{bypass: bypass} do
      error = %{"extensions" => %{"code" => "UNAUTHORIZED"}}
      response = %{"data" => nil, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      hub = build(:fly)
      assert {:error, "request failed with code: UNAUTHORIZED"} = FlyClient.fetch_app(hub)
    end
  end

  describe "put_secrets/2" do
    test "puts a list of secrets inside application", %{bypass: bypass} do
      secrets = [
        %{
          "createdAt" => to_string(DateTime.utc_now()),
          "digest" => to_string(Livebook.Utils.random_cookie()),
          "id" => Livebook.Utils.random_short_id(),
          "name" => "FOO"
        }
      ]

      response = %{"data" => %{"setSecrets" => %{"app" => %{"secrets" => secrets}}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      hub = build(:fly)
      assert {:ok, ^secrets} = FlyClient.put_secrets(hub, [%{key: "FOO", value: "BAR"}])
    end

    test "returns error when input is invalid", %{bypass: bypass} do
      message =
        "Variable $input of type SetSecretsInput! was provided invalid value for secrets.0.Value (Field is not defined on SecretInput), secrets.0.value (Expected value to not be null)"

      error = %{
        "extensions" => %{
          "problems" => [
            %{
              "explanation" => "Field is not defined on SecretInput",
              "path" => ["secrets", 0, "Value"]
            },
            %{
              "explanation" => "Expected value to not be null",
              "path" => ["secrets", 0, "value"]
            }
          ],
          "value" => %{
            "appId" => "myfoo-test-livebook",
            "secrets" => [%{"Value" => "BAR", "key" => "FOO"}]
          }
        },
        "locations" => [%{"column" => 10, "line" => 1}],
        "message" => message
      }

      response = %{"data" => nil, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      hub = build(:fly)
      assert {:error, ^message} = FlyClient.put_secrets(hub, [%{key: "FOO", Value: "BAR"}])
    end

    test "returns unauthorized when token is invalid", %{bypass: bypass} do
      error = %{"extensions" => %{"code" => "UNAUTHORIZED"}}
      response = %{"data" => nil, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      hub = build(:fly)

      assert {:error, "request failed with code: UNAUTHORIZED"} =
               FlyClient.put_secrets(hub, [%{key: "FOO", value: "BAR"}])
    end
  end

  describe "delete_secrets/2" do
    test "deletes a list of secrets inside application", %{bypass: bypass} do
      response = %{"data" => %{"unsetSecrets" => %{"app" => %{"secrets" => []}}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      hub = build(:fly)
      assert {:ok, []} = FlyClient.delete_secrets(hub, ["FOO"])
    end

    test "returns unauthorized when token is invalid", %{bypass: bypass} do
      error = %{"extensions" => %{"code" => "UNAUTHORIZED"}}
      response = %{"data" => nil, "errors" => [error]}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      hub = build(:fly)

      assert {:error, "request failed with code: UNAUTHORIZED"} =
               FlyClient.delete_secrets(hub, ["FOO"])
    end
  end
end
