defmodule LivebookWeb.AuthPlugTest do
  use LivebookWeb.ConnCase, async: false

  setup context do
    if context[:token] do
      Application.put_env(:livebook, :token, context[:token])

      on_exit(fn ->
        Application.delete_env(:livebook, :token)
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
end
