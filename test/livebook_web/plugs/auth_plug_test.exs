defmodule LivebookWeb.AuthPlugTest do
  use ExUnit.Case, async: false
  use Plug.Test

  defmodule MyPlug do
    use Plug.Builder

    plug Plug.Session,
      store: :cookie,
      key: "test",
      signing_salt: "test",
      secret_key_base: "9hHHeOiAA8wrivUfuS//jQMurHxoMYUtF788BQMx2KO7mYUE8rVrGGG09djBNQq7"

    plug :fetch_session
    plug :fetch_query_params
    plug LivebookWeb.AuthPlug
    plug :passthrough

    defp passthrough(conn, _), do: Plug.Conn.send_resp(conn, 404, "Passthrough")
  end

  defp call(conn), do: MyPlug.call(conn, [])

  setup context do
    if context[:token] do
      Application.put_env(:livebook, :token, context[:token])

      on_exit(fn ->
        Application.delete_env(:livebook, :token)
      end)
    else
      :ok
    end
  end

  describe "token authentication" do
    test "skips authentication when no token is configured" do
      conn = conn(:get, "/") |> call()

      assert conn.status == 404
      assert conn.resp_body == "Passthrough"
    end

    @tag token: "grumpycat"
    test "returns authentication error when token is set and none provided" do
      conn = conn(:get, "/") |> call()

      assert conn.status == 401
      assert conn.resp_body == "Unauthorized"
    end

    @tag token: "grumpycat"
    test "redirects to the same path when valid token is provided in query params" do
      conn = conn(:get, "/?token=grumpycat") |> call()

      assert Phoenix.ConnTest.redirected_to(conn) == "/"
    end

    @tag token: "grumpycat"
    test "returns authentication error when invalid token is provided in query params" do
      conn = conn(:get, "/?token=invalid") |> call()

      assert conn.status == 401
      assert conn.resp_body == "Unauthorized"
    end

    @tag token: "grumpycat"
    test "passes authentication when valid token is provided in session" do
      conn =
        conn(:get, "/")
        |> init_test_session(%{"80:token" => "grumpycat"})
        |> call()

      assert conn.status == 404
      assert conn.resp_body == "Passthrough"
    end

    @tag token: "grumpycat"
    test "returns authentication error when invalid token is provided in session" do
      conn =
        conn(:get, "/")
        |> init_test_session(%{"80:token" => "invalid"})
        |> call()

      assert conn.status == 401
      assert conn.resp_body == "Unauthorized"
    end
  end
end
