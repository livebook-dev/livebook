defmodule LivebookWeb.AuthPlugTest do
  use LivebookWeb.ConnCase, async: false

  setup context do
    {type, value} =
      cond do
        token = context[:token] -> {:token, token}
        password = context[:password] -> {:password, password}
        true -> {:disabled, ""}
      end

    unless type == :disabled do
      Application.put_env(:livebook, :authentication_mode, type)
      Application.put_env(:livebook, type, value)

      on_exit(fn ->
        Application.put_env(:livebook, :authentication_mode, :disabled)
        Application.delete_env(:livebook, type)
      end)
    end

    :ok
  end

  describe "token authentication" do
    test "skips authentication when no token is configured", %{conn: conn} do
      conn = get(conn, "/")

      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"
    end

    @tag token: "grumpycat"
    test "redirects to '/authenticate' if not authenticated", %{conn: conn} do
      conn = get(conn, "/")
      assert redirected_to(conn) == "/authenticate"
    end

    @tag token: "grumpycat"
    test "redirects to the same path when valid token is provided in query params", %{conn: conn} do
      conn = get(conn, "/?token=grumpycat")

      assert redirected_to(conn) == "/"
    end

    @tag token: "grumpycat"
    test "redirects to '/authenticate' when invalid token is provided in query params",
         %{conn: conn} do
      conn = get(conn, "/")
      assert redirected_to(conn) == "/authenticate"
    end

    @tag token: "grumpycat"
    test "persists authentication across requests", %{conn: conn} do
      conn = get(conn, "/?token=grumpycat")
      assert get_session(conn, "80:token")

      conn = get(conn, "/")
      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"
    end

    @tag token: "grumpycat"
    test "redirects to referer on valid authentication", %{conn: conn} do
      referer = "/import?url=example.com"

      conn = get(conn, referer)
      assert redirected_to(conn) == "/authenticate"

      conn = post(conn, "/authenticate", token: "grumpycat")
      assert redirected_to(conn) == referer
    end

    @tag token: "grumpycat"
    test "redirects back to '/authenticate' on invalid token", %{conn: conn} do
      conn = post(conn, Routes.auth_path(conn, :authenticate), token: "invalid token")
      assert html_response(conn, 200) =~ "Authentication required"

      conn = get(conn, "/")
      assert redirected_to(conn) == "/authenticate"
    end

    @tag token: "grumpycat"
    test "persists authentication across requests (via /authenticate)", %{conn: conn} do
      conn = post(conn, Routes.auth_path(conn, :authenticate), token: "grumpycat")
      assert get_session(conn, "80:token")

      conn = get(conn, "/")
      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"

      conn = get(conn, "/authenticate")
      assert redirected_to(conn) == "/"
    end
  end

  describe "password authentication" do
    test "redirects to '/' if no authentication is required", %{conn: conn} do
      conn = get(conn, "/authenticate")
      assert redirected_to(conn) == "/"
    end

    @tag password: "grumpycat"
    test "redirects to '/authenticate' if not authenticated", %{conn: conn} do
      conn = get(conn, "/")
      assert redirected_to(conn) == "/authenticate"
    end

    @tag password: "grumpycat"
    test "redirects to '/' on valid authentication", %{conn: conn} do
      conn = post(conn, Routes.auth_path(conn, :authenticate), password: "grumpycat")
      assert redirected_to(conn) == "/"

      conn = get(conn, "/")
      assert html_response(conn, 200) =~ "New notebook"
    end

    @tag password: "grumpycat"
    test "redirects to referer on valid authentication", %{conn: conn} do
      referer = "/import?url=example.com"

      conn = get(conn, referer)
      assert redirected_to(conn) == "/authenticate"

      conn = post(conn, "/authenticate", password: "grumpycat")
      assert redirected_to(conn) == referer
    end

    @tag password: "grumpycat"
    test "redirects back to '/authenticate' on invalid password", %{conn: conn} do
      conn = post(conn, Routes.auth_path(conn, :authenticate), password: "invalid password")
      assert html_response(conn, 200) =~ "Authentication required"

      conn = get(conn, "/")
      assert redirected_to(conn) == "/authenticate"
    end

    @tag password: "grumpycat"
    test "persists authentication across requests", %{conn: conn} do
      conn = post(conn, Routes.auth_path(conn, :authenticate), password: "grumpycat")
      assert get_session(conn, "80:password")

      conn = get(conn, "/")
      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"

      conn = get(conn, "/authenticate")
      assert redirected_to(conn) == "/"
    end
  end
end
