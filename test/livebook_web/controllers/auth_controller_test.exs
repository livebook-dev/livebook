defmodule LivebookWeb.AuthControllerTest do
  use LivebookWeb.ConnCase, async: true

  @credentials Livebook.Utils.random_id()

  describe "authenticate" do
    setup tags do
      if auth_mode = tags[:auth_mode],
        do: Application.put_env(:livebook, :authentication_mode, auth_mode)

      on_exit(fn ->
        Application.put_env(:livebook, :authentication_mode, :disabled)
        Application.delete_env(:livebook, :password)
      end)
    end

    test "loads the home page if the authentication mode is disabled", %{conn: conn} do
      conn = get(conn, Routes.auth_path(conn, :index))
      assert redirected_to(conn) == "/"
    end

    @tag auth_mode: :password
    test "redirects to home page when password is correctly", %{conn: conn} do
      Application.put_env(:livebook, :password, @credentials)

      conn = post(conn, Routes.auth_path(conn, :authenticate), %{password: @credentials})
      assert redirected_to(conn) == "/"
    end

    @tag auth_mode: :password
    test "shows password error message when it's invalid", %{conn: conn} do
      Application.put_env(:livebook, :password, @credentials)

      conn = post(conn, Routes.auth_path(conn, :authenticate), %{password: "foo"})

      assert html_response(conn, 200) =~ ~s/<div phx-feedback-for="password" class="show-errors">/
      assert html_response(conn, 200) =~ "password is invalid"
    end

    @tag auth_mode: :token
    test "redirects to home page when token is correctly", %{conn: conn} do
      Application.put_env(:livebook, :token, @credentials)

      conn = post(conn, Routes.auth_path(conn, :authenticate), %{token: @credentials})
      assert redirected_to(conn) == "/"
    end

    @tag auth_mode: :token
    test "shows token error message when it's invalid", %{conn: conn} do
      Application.put_env(:livebook, :token, @credentials)

      conn = post(conn, Routes.auth_path(conn, :authenticate), %{token: "foo"})

      assert html_response(conn, 200) =~ ~s/<div phx-feedback-for="token" class="show-errors">/
      assert html_response(conn, 200) =~ "token is invalid"
    end
  end
end
