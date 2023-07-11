defmodule Livebook.Zta.GoogleIapTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias Livebook.ZTA.GoogleIAP

  setup do
    bypass = Bypass.open()

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
      identity: %{iss: "livebook", key: "livebookweb"},
      keys: nil
    ]

    fields = [:id, :name, :email]

    Bypass.expect(bypass, fn conn ->
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(200, Jason.encode!(%{keys: [key]}))
    end)

    {:ok, pid} = GenServer.start_link(GoogleIAP, options)

    {:ok, bypass: bypass, key: key, token: token, options: options, fields: fields, pid: pid}
  end

  test "returns the user when it's valid", %{pid: pid, token: token, fields: fields} do
    user = GenServer.call(pid, {:authenticate, [token], fields: fields})
    assert %{id: "1234567890", email: "tuka@peralta.com"} = user
  end

  test "returns nil when the identity is invalid", setup do
    options = Keyword.put(setup.options, :identity, %{iss: "invalid_iss", key: "livebookweb"})
    {:ok, pid} = GenServer.start_link(GoogleIAP, options)
    assert GenServer.call(pid, {:authenticate, [setup.token], fields: setup.fields}) == nil
  end

  test "returns nil when the token is invalid", %{pid: pid, fields: fields} do
    assert GenServer.call(pid, {:authenticate, ["invalid_token"], fields: fields}) == nil
    assert GenServer.call(pid, {:authenticate, "invalid_token", fields: fields}) == nil
    assert GenServer.call(pid, {:authenticate, [], fields: fields}) == nil
    assert GenServer.call(pid, {:authenticate, nil, fields: fields}) == nil
  end

  test "returns nil when the key is invalid", setup do
    Bypass.expect_once(setup.bypass, fn conn ->
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(200, Jason.encode!(%{keys: ["invalid_key"]}))
    end)

    {:ok, pid} = GenServer.start_link(GoogleIAP, setup.options)

    assert GenServer.call(pid, {:authenticate, [setup.token], fields: setup.fields}) == nil
  end
end
