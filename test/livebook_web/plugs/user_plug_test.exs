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
      |> fetch_cookies()
      |> call()

    assert get_session(conn, :user_id) != nil
  end

  test "keeps user id in the session if present" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{user_id: "valid_user_id"})
      |> fetch_cookies()
      |> call()

    assert get_session(conn, :user_id) != nil
  end

  test "given no user_data cookie, generates and stores new data" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{})
      |> fetch_cookies()
      |> call()

    assert %{
             "name" => nil,
             "hex_color" => <<_::binary>>
           } = conn.cookies["lb_user_data"] |> Base.decode64!() |> JSON.decode!()
  end

  test "keeps user_data cookie if present" do
    cookie_value =
      %{name: "Jake Peralta", hex_color: "#000000"} |> JSON.encode!() |> Base.encode64()

    conn =
      conn(:get, "/")
      |> init_test_session(%{})
      |> put_req_cookie("lb_user_data", cookie_value)
      |> fetch_cookies()
      |> call()

    assert conn.cookies["lb_user_data"] == cookie_value
  end

  test "assigns identity data and user data" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{})
      |> fetch_cookies()
      |> call()

    assert %{identity_data: %{}, user_data: %{}} = conn.assigns
  end
end
