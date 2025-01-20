defmodule LivebookWeb.UserControllerTest do
  use LivebookWeb.ConnCase, async: true

  describe "GET /logout" do
    test "renders logout template when logged in", %{conn: conn} do
      conn = login_user(conn)

      conn = get(conn, ~p"/logout")

      assert html_response(conn, 200) =~ "You have been logged out"
    end

    test "redirects when already logged out", %{conn: conn} do
      conn = logout_user(conn)

      conn = get(conn, ~p"/logout")

      assert redirected_to(conn) == ~p"/"
    end

    defp login_user(conn) do
      Phoenix.ConnTest.init_test_session(conn, %{user_id: 1})
    end

    defp logout_user(conn) do
      Phoenix.ConnTest.init_test_session(conn, %{})
    end
  end
end
