defmodule Livebook.ZTA.TailscaleTest do
  use ExUnit.Case, async: true
  use Plug.Test
  alias Livebook.ZTA.Tailscale

  @moduletag unix: true
  @fields [:id, :name, :email]
  @name Context.Test.Tailscale
  @path "/localapi/v0/whois"

  def valid_user_response(conn) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(
      200,
      JSON.encode!(%{
        UserProfile: %{
          ID: 1_234_567_890,
          DisplayName: "John",
          LoginName: "john@example.org"
        }
      })
    )
  end

  setup do
    bypass = Bypass.open()

    conn = %Plug.Conn{conn(:get, @path) | remote_ip: {151, 236, 219, 228}}

    options = [
      name: @name,
      identity_key: "http://localhost:#{bypass.port}"
    ]

    {:ok, bypass: bypass, options: options, conn: conn}
  end

  test "returns the user when it's valid", %{bypass: bypass, options: options, conn: conn} do
    Bypass.expect(bypass, fn conn ->
      assert %{"addr" => "151.236.219.228:1"} = conn.query_params
      valid_user_response(conn)
    end)

    start_supervised!({Tailscale, options})
    {_conn, user} = Tailscale.authenticate(@name, conn, @fields)
    assert %{id: "1234567890", email: "john@example.org", name: "John"} = user
  end

  defmodule TestPlug do
    def init(options), do: options
    def call(conn, _opts), do: Livebook.ZTA.TailscaleTest.valid_user_response(conn)
  end

  @tag :tmp_dir
  test "returns valid user via unix socket", %{options: options, conn: conn, tmp_dir: tmp_dir} do
    socket = Path.relative_to_cwd("#{tmp_dir}/bandit.sock")
    options = Keyword.put(options, :identity_key, socket)
    start_supervised!({Bandit, plug: TestPlug, ip: {:local, socket}, port: 0})
    start_supervised!({Tailscale, options})
    {_conn, user} = Tailscale.authenticate(@name, conn, @fields)
    assert %{id: "1234567890", email: "john@example.org", name: "John"} = user
  end

  test "raises when configured with missing unix socket", %{options: options} do
    Process.flag(:trap_exit, true)
    options = Keyword.put(options, :identity_key, "./invalid-socket.sock")

    assert ExUnit.CaptureLog.capture_log(fn ->
             {:error, _} = start_supervised({Tailscale, options})
           end) =~ "Tailscale socket does not exist"
  end

  test "returns nil when it's invalid", %{bypass: bypass, options: options} do
    Bypass.expect_once(bypass, fn conn ->
      assert %{"addr" => "151.236.219.229:1"} = conn.query_params

      conn
      |> send_resp(404, "no match for IP:port")
    end)

    conn = %Plug.Conn{conn(:get, @path) | remote_ip: {151, 236, 219, 229}}

    start_supervised!({Tailscale, options})
    assert {_conn, nil} = Tailscale.authenticate(@name, conn, @fields)
  end

  test "includes an authorization header when userinfo is provided", %{
    bypass: bypass,
    options: options,
    conn: conn
  } do
    options = Keyword.put(options, :identity_key, "http://:foobar@localhost:#{bypass.port}")

    Bypass.expect_once(bypass, fn conn ->
      assert %{"addr" => "151.236.219.228:1"} = conn.query_params
      assert Plug.Conn.get_req_header(conn, "authorization") == ["Basic OmZvb2Jhcg=="]

      conn
      |> send_resp(404, "no match for IP:port")
    end)

    start_supervised!({Tailscale, options})
    assert {_conn, nil} = Tailscale.authenticate(@name, conn, @fields)
  end
end
