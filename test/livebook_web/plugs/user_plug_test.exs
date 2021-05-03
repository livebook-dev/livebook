defmodule LivebookWeb.UserPlugTest do
  use ExUnit.Case, async: true
  use Plug.Test

  defp call(conn) do
    LivebookWeb.UserPlug.call(conn, LivebookWeb.UserPlug.init([]))
  end

  test "given no user id in the session, generates a new user id" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{})
      |> call()

    assert get_session(conn, :current_user_id) != nil
  end

  test "keeps user id in the session if present" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{current_user_id: "valid_user_id"})
      |> call()

    assert get_session(conn, :current_user_id) != nil
  end

  test "given no user_data cookie, generates and stores new data" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{})
      |> fetch_cookies()
      |> call()
      |> fetch_cookies()

    assert conn.cookies["user_data"] != nil
  end

  test "keeps user_data cookie if present" do
    cookie_value = ~s/{"name":"Jake Peralta","hex_color":"#000000"}/

    conn =
      conn(:get, "/")
      |> init_test_session(%{})
      |> put_req_cookie("user_data", cookie_value)
      |> fetch_cookies()
      |> call()
      |> fetch_cookies()

    assert conn.cookies["user_data"] == cookie_value
  end
end
