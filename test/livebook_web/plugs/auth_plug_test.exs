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
    test "returns authentication error when token is set and none provided", %{conn: conn} do
      {_, _, resp_body} =
        assert_error_sent 401, fn ->
          get(conn, "/")
        end

      assert resp_body =~ "Authentication required"
    end

    @tag token: "grumpycat"
    test "redirects to the same path when valid token is provided in query params", %{conn: conn} do
      conn = get(conn, "/?token=grumpycat")

      assert redirected_to(conn) == "/"
    end

    @tag token: "grumpycat"
    test "returns authentication error when invalid token is provided in query params",
         %{conn: conn} do
      {_, _, resp_body} =
        assert_error_sent 401, fn ->
          get(conn, "/?token=invalid")
        end

      assert resp_body =~ "Authentication required"
    end

    @tag token: "grumpycat"
    test "persists authentication across requests using cookies", %{conn: conn} do
      conn = get(conn, "/?token=grumpycat")

      assert Map.has_key?(conn.resp_cookies, "80:token")

      conn =
        build_conn()
        |> Plug.Test.recycle_cookies(conn)
        |> get("/")

      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"
    end
  end

  describe "password authentication" do
    @tag password: "grumpycat"
    test "redirects to '/authenticate' if not already authenticated", %{conn: conn} do
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
    test "redirects back to '/authenticate' on invalid password", %{conn: conn} do
      conn = post(conn, Routes.auth_path(conn, :authenticate), password: "invalid password")

      conn = get(conn, "/")
      assert redirected_to(conn) == "/authenticate"
    end

    @tag password: "grumpycat"
    test "persists authentication across requests using cookies", %{conn: conn} do
      conn = post(conn, Routes.auth_path(conn, :authenticate), password: "grumpycat")

      assert Map.has_key?(conn.resp_cookies, "80:password")

      conn =
        build_conn()
        |> Plug.Test.recycle_cookies(conn)
        |> get("/")

      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"
    end
  end
end
