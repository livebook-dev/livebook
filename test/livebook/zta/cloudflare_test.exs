defmodule Livebook.ZTA.CloudflareTest do
  use ExUnit.Case, async: true
  import Plug.Test
  import Plug.Conn

  alias Livebook.ZTA.Cloudflare

  @fields [:id, :name, :email]
  @name Context.Test.Claudflare

  setup do
    bypass = Bypass.open()
    user_identity = Bypass.open()

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
      name: @name,
      custom_identity: %{
        iss: "livebook",
        key: "livebookweb",
        certs: "http://localhost:#{bypass.port}",
        user_identity: "http://localhost:#{user_identity.port}"
      }
    ]

    Bypass.expect(bypass, fn conn ->
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(200, JSON.encode!(%{keys: [key]}))
    end)

    conn = conn(:get, "/") |> put_req_header("cf-access-jwt-assertion", token)

    {:ok,
     bypass: bypass, token: token, options: options, user_identity: user_identity, conn: conn}
  end

  test "returns the user_identity when the user is valid", context do
    expected_user = %{
      "user_uuid" => "1234567890",
      "name" => "Tuka Peralta",
      "email" => "tuka@peralta.com"
    }

    Bypass.expect_once(context.user_identity, fn conn ->
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(200, JSON.encode!(expected_user))
    end)

    start_supervised!({Cloudflare, context.options})
    {_conn, user} = Cloudflare.authenticate(@name, context.conn, fields: @fields)

    assert %{id: "1234567890", email: "tuka@peralta.com", name: "Tuka Peralta", payload: %{}} =
             user
  end

  test "returns nil when the user_identity fails", context do
    Bypass.expect_once(context.user_identity, fn conn ->
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(403, "")
    end)

    start_supervised!({Cloudflare, context.options})

    assert {_conn, nil} = Cloudflare.authenticate(@name, context.conn, fields: @fields)
  end

  test "returns nil when the iss is invalid", %{options: options, conn: conn} do
    invalid_identity = Map.replace(options[:custom_identity], :iss, "invalid_iss")
    options = Keyword.put(options, :custom_identity, invalid_identity)
    start_supervised!({Cloudflare, options})

    assert {_conn, nil} = Cloudflare.authenticate(@name, conn, fields: @fields)
  end

  test "returns nil when the token is invalid", %{options: options} do
    conn = conn(:get, "/") |> put_req_header("cf-access-jwt-assertion", "invalid_token")
    start_supervised!({Cloudflare, options})

    assert {_conn, nil} = Cloudflare.authenticate(@name, conn, fields: @fields)
  end

  test "returns nil when the assertion is invalid", %{options: options, token: token} do
    conn = conn(:get, "/") |> put_req_header("invalid_assertion", token)
    start_supervised!({Cloudflare, options})

    assert {_conn, nil} = Cloudflare.authenticate(@name, conn, fields: @fields)
  end

  test "returns nil when the key is invalid", %{bypass: bypass, options: options, conn: conn} do
    Bypass.expect_once(bypass, fn conn ->
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(200, JSON.encode!(%{keys: ["invalid_key"]}))
    end)

    start_supervised!({Cloudflare, options})

    assert {_conn, nil} = Cloudflare.authenticate(@name, conn, fields: @fields)
  end
end
