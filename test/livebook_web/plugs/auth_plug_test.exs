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
      conn = get(conn, "/")

      assert conn.status == 401
      assert conn.resp_body =~ "Authentication required"
    end

    @tag token: "grumpycat"
    test "redirects to the same path when valid token is provided in query params", %{conn: conn} do
      conn = get(conn, "/?token=grumpycat")

      assert redirected_to(conn) == "/"
    end

    @tag token: "grumpycat"
    test "returns authentication error when invalid token is provided in query params",
         %{conn: conn} do
      conn = get(conn, "/?token=invalid")

      assert conn.status == 401
      assert conn.resp_body =~ "Authentication required"
    end

    @tag token: "grumpycat"
    test "passes authentication when valid token is provided in session", %{conn: conn} do
      conn =
        conn
        |> init_test_session(%{"80:token" => "grumpycat"})
        |> get("/")

      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"
    end

    @tag token: "grumpycat"
    test "returns authentication error when invalid token is provided in session", %{conn: conn} do
      conn =
        conn
        |> init_test_session(%{"80:token" => "invalid"})
        |> get("/")

      assert conn.status == 401
      assert conn.resp_body =~ "Authentication required"
    end
  end
end
