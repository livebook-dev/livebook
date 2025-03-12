defmodule LivebookWeb.UserControllerTest do
  use LivebookWeb.ConnCase, async: true

  describe "GET /logout" do
    test "redirects when already logged out", %{conn: conn} do
      assert conn
             |> Phoenix.ConnTest.init_test_session(%{})
             |> get(~p"/logout")
             |> redirected_to() == ~p"/"
    end
  end
end
