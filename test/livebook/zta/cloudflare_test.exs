defmodule Livebook.Zta.CloudflareTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias Livebook.ZTA.Cloudflare

  setup do
    bypass = Bypass.open()
    user_identity = Bypass.open(port: 1234)

    key = %{
      "kty" => "RSA",
      "e" => "AQAB",
      "use" => "sig",
      "kid" => "bmlyt6y2uWrgWeUh3mENiSkEOR7Np3I8swSjlK98iX0",
      "alg" => "RS256",
      "n" =>
        "qvMgmj7GrjMAKxib9ODcdNyMwhsU1jwjvyAANrCJ5n1UcM82lZ5B3YP13zbPY3vRuufkW_GuA2cEZ8htMGT79kMsPz1cLrwIeUNOdGzncQQvBJVmQgw8NOuflVy5OajvfSe4a5PQmpC6BEp1d-Ix0S4BV2vWJUb0UtHg3bM4GgHTrnhHkSyXfpSZT4SNqnSOtiXiD-7lue52cPlZotkeTR2D4LTVSrsCdp21wGvAxXqnfpRcKYs5EyEmyTQ85zak7nBAReMrAqrRilXej8qTWGGIg1TRILvoCMd3nF5QjcjRCx2JMMHXG4tZNoK4QbEQlsdcd45B1VpE15TwgNTx4Q"
    }

    token =
      "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiZW1haWwiOiJ0dWthQHBlcmFsdGEuY29tIiwiaWF0IjoxNTE2MjM5MDIyLCJpc3MiOiJsaXZlYm9vayIsImF1ZCI6ImxpdmVib29rd2ViIn0.ZP5LIrkfMHq2p8g3SMgC7RBt7899GeHkP9rzYKM3RvjDCBeYoxpeioLW2sXMT74QyJPxB4JUujRU3shSPIWNAxkjJVaBGwVTqsO_PR34DSx82q45qSkUnkSVXLl-2KgN4BoUUK7dmocP6yzhNQ3XGf6669n5UG69eMZdh9PApZ7GuyRUQ80ubpvakWaIpd9PIaORkptqDWVbyOIk3Z79AMUub0MSG1FpzYByAQoLswob24l2xVo95-aQrdatqLk1sJ43AZ6HLoMxkZkWobYYRMH5w65MkQckJ9NzI3Rk-VOUlg9ePo8OPRnvcGY-OozHXrjdzn2-j03xuP6x1J3Y7Q"

    options = [
      req_options: [url: "http://localhost:#{bypass.port}"],
      identity: %{
        iss: "livebook",
        key: "livebookweb",
        user_identity: "http://localhost:#{user_identity.port}"
      },
      keys: nil
    ]

    fields = [:id, :name, :email]

    {:ok,
     bypass: bypass,
     key: key,
     token: token,
     options: options,
     fields: fields,
     user_identity: user_identity}
  end

  test "returns the user when it's valid", setup do
    Bypass.expect_once(setup.bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(200, Jason.encode!(%{keys: [setup.key]}))
    end)

    expected_user = %{
      "user_uuid" => "1234567890",
      "name" => "Tuka Peralta",
      "email" => "tuka@peralta.com"
    }

    Bypass.expect(setup.user_identity, fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(200, Jason.encode!(expected_user))
    end)

    {:ok, pid} = GenServer.start(Cloudflare, setup.options)

    user = GenServer.call(pid, {:authenticate, [setup.token], fields: setup.fields})

    assert %{id: "1234567890", email: "tuka@peralta.com", name: "Tuka Peralta"} = user
  end

  test "returns nil when the user_identity fails", setup do
    Bypass.expect_once(setup.bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(200, Jason.encode!(%{keys: [setup.key]}))
    end)

    Bypass.expect(setup.user_identity, fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(403, "")
    end)

    {:ok, pid} = GenServer.start(Cloudflare, setup.options)
    assert GenServer.call(pid, {:authenticate, [setup.token], fields: setup.fields}) == nil
  end
end
