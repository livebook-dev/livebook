defmodule LivebookWeb.AuthPlugTest do
  use LivebookWeb.ConnCase, async: true

  test "skips authentication when it is disabled", %{conn: conn} do
    conn = get(conn, ~p"/")

    assert conn.status == 200
    assert conn.resp_body =~ "New notebook"
  end

  test "/authenticate redirects to / when authentication is disabled", %{conn: conn} do
    conn = get(conn, ~p"/authenticate")
    assert redirected_to(conn) == ~p"/"
  end

  describe "token authentication" do
    @describetag authentication: %{mode: :token, secret: "grumpycat"}

    test "redirects if not authenticated", %{conn: conn} do
      conn = get(conn, ~p"/")
      assert redirected_to(conn) in unauthenticated_homes()
    end

    test "redirects to the same path when valid token is provided in query params", %{conn: conn} do
      conn = get(conn, ~p"/?token=grumpycat")

      assert redirected_to(conn) == ~p"/"
    end

    test "redirects when invalid token is provided in query params", %{conn: conn} do
      conn = get(conn, ~p"/?token=invalid")
      assert redirected_to(conn) in unauthenticated_homes()
    end

    test "persists authentication across requests", %{conn: conn} do
      conn = get(conn, ~p"/?token=grumpycat")
      assert get_session(conn, "80:token")

      conn = get(conn, ~p"/")
      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"
    end

    test "redirects to referer on valid authentication", %{conn: conn} do
      referer = "/import?url=example.com"

      conn = get(conn, referer)
      assert redirected_to(conn) == ~p"/authenticate"

      conn = post(conn, ~p"/authenticate", token: "grumpycat")
      assert redirected_to(conn) == referer
    end

    test "redirects back to /authenticate on invalid token", %{conn: conn} do
      conn = post(conn, ~p"/authenticate?token=invalid_token")
      assert html_response(conn, 200) =~ "Authentication required"

      conn = get(conn, ~p"/")
      assert redirected_to(conn) in unauthenticated_homes()
    end

    test "persists authentication across requests (via /authenticate)", %{conn: conn} do
      conn = post(conn, ~p"/authenticate?token=grumpycat")
      assert get_session(conn, "80:token")

      conn = get(conn, ~p"/")
      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"

      conn = get(conn, ~p"/authenticate")
      assert redirected_to(conn) == ~p"/"
    end
  end

  describe "password authentication" do
    @describetag authentication: %{mode: :password, secret: "grumpycat"}

    test "does not crash when given a token", %{conn: conn} do
      conn = post(conn, ~p"/authenticate?token=grumpycat")
      assert html_response(conn, 200) =~ "token is invalid"
    end

    test "redirects if not authenticated", %{conn: conn} do
      conn = get(conn, ~p"/")
      assert redirected_to(conn) in unauthenticated_homes()
    end

    test "redirects to / on valid authentication", %{conn: conn} do
      conn = post(conn, ~p"/authenticate?password=grumpycat")
      assert redirected_to(conn) == ~p"/"

      conn = get(conn, ~p"/")
      assert html_response(conn, 200) =~ "New notebook"
    end

    test "redirects to referer on valid authentication", %{conn: conn} do
      referer = "/import?url=example.com"

      conn = get(conn, referer)
      assert redirected_to(conn) == ~p"/authenticate"

      conn = post(conn, ~p"/authenticate", password: "grumpycat")
      assert redirected_to(conn) == referer
    end

    test "redirects back to /authenticate on invalid password", %{conn: conn} do
      conn = post(conn, ~p"/authenticate?password=invalid_password")
      assert html_response(conn, 200) =~ "Authentication required"

      conn = get(conn, ~p"/")
      assert redirected_to(conn) in unauthenticated_homes()
    end

    test "persists authentication across requests", %{conn: conn} do
      conn = post(conn, ~p"/authenticate?password=grumpycat")
      assert get_session(conn, "80:password")

      conn = get(conn, ~p"/")
      assert conn.status == 200
      assert conn.resp_body =~ "New notebook"

      conn = get(conn, ~p"/authenticate")
      assert redirected_to(conn) == ~p"/"
    end
  end

  defp unauthenticated_homes(), do: [~p"/authenticate", ~p"/apps"]
end
