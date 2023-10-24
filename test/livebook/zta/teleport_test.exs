defmodule Livebook.ZTA.TeleportTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias Livebook.ZTA.Teleport

  @fields [:id, :name, :email]
  @name Context.Test.Teleport

  @public_key """
  -----BEGIN PUBLIC KEY-----
  MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEEVs/o5+uQbTjL3chynL4wXgUg2R9
  q9UU8I5mEovUf86QZ7kOBIjJwqnzD1omageEHWwHdBO6B+dFabmdT9POxg==
  -----END PUBLIC KEY-----
  """

  @private_key """
  -----BEGIN PRIVATE KEY-----
  MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgevZzL1gdAFr88hb2
  OF/2NxApJCzGCEDdfSp6VQO30hyhRANCAAQRWz+jn65BtOMvdyHKcvjBeBSDZH2r
  1RTwjmYSi9R/zpBnuQ4EiMnCqfMPWiZqB4QdbAd0E7oH50VpuZ1P087G
  -----END PRIVATE KEY-----
  """

  setup do
    bypass = Bypass.open()

    options = [
      name: @name,
      identity: %{
        key: "http://localhost:#{bypass.port}"
      }
    ]

    Bypass.expect(bypass, "GET", "/.well-known/jwks.json", fn conn ->
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(200, Jason.encode!(%{keys: get_well_known_jwks()}))
    end)

    token = create_token()

    conn = conn(:get, "/") |> put_req_header("teleport-jwt-assertion", token)

    {:ok, bypass: bypass, options: options, conn: conn, token: token}
  end

  test "returns the user when it's valid", %{options: options, conn: conn} do
    start_supervised!({Teleport, options})
    {_conn, user} = Teleport.authenticate(@name, conn, fields: @fields)
    assert %{id: "my-user-id", username: "myusername"} = user
  end

  test "returns nil when the exp is in the past", %{options: options, conn: conn} do
    iat = DateTime.utc_now() |> DateTime.add(-10000)
    exp = DateTime.utc_now() |> DateTime.add(-1000)
    conn = put_req_header(conn, "teleport-jwt-assertion", create_token(iat, exp))
    start_supervised!({Teleport, options})

    assert {_conn, nil} = Teleport.authenticate(@name, conn, fields: @fields)
  end

  test "returns nil when the nbf is not reached yet", %{options: options, conn: conn} do
    iat = DateTime.utc_now() |> DateTime.add(1000)
    exp = DateTime.utc_now() |> DateTime.add(10000)
    conn = put_req_header(conn, "teleport-jwt-assertion", create_token(iat, exp))
    start_supervised!({Teleport, options})

    assert {_conn, nil} = Teleport.authenticate(@name, conn, fields: @fields)
  end

  test "returns nil when the token is invalid", %{options: options} do
    conn = conn(:get, "/") |> put_req_header("teleport-jwt-assertion", "invalid_token")
    start_supervised!({Teleport, options})

    assert {_conn, nil} = Teleport.authenticate(@name, conn, fields: @fields)
  end

  test "returns nil when the assertion is invalid", %{options: options} do
    conn = conn(:get, "/") |> put_req_header("invalid_assertion", create_token())
    start_supervised!({Teleport, options})

    assert {_conn, nil} = Teleport.authenticate(@name, conn, fields: @fields)
  end

  test "fails to start the process when the key is invalid", %{bypass: bypass, options: options} do
    Bypass.expect(bypass, "GET", "/.well-known/jwks.json", fn conn ->
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(200, Jason.encode!(%{keys: ["invalid_key"]}))
    end)

    assert_raise RuntimeError, fn ->
      start_supervised!({Teleport, options})
    end
  end

  defp get_well_known_jwks() do
    jwk = @public_key |> JOSE.JWK.from_pem() |> JOSE.JWK.to_map() |> elem(1) |> Map.put("kid", "")
    [jwk]
  end

  defp create_token(
         iat \\ DateTime.utc_now(),
         exp \\ DateTime.add(DateTime.utc_now(), 1000)
       ) do
    iat = DateTime.to_unix(iat)
    exp = DateTime.to_unix(exp)

    payload = %{
      "aud" => ["http://localhost:4000"],
      "exp" => exp,
      "iat" => iat,
      "iss" => "my-teleport-custer",
      "nbf" => iat,
      "roles" => ["access", "editor", "member"],
      "sub" => "my-user-id",
      "traits" => %{"host_user_gid" => [""], "host_user_uid" => [""]},
      "username" => "myusername"
    }

    @private_key
    |> JOSE.JWK.from_pem()
    |> JOSE.JWT.sign(payload)
    |> JOSE.JWS.compact()
    |> elem(1)
  end
end
