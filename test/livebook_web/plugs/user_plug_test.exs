defmodule LivebookWeb.UserPlugTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias Livebook.Users
  alias Livebook.Users.User

  defp call(conn) do
    LivebookWeb.UserPlug.call(conn, LivebookWeb.UserPlug.init([]))
  end

  test "given no user id in the session, generates a new user" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{})
      |> call()

    user_id = get_session(conn, :current_user_id)
    assert Users.exists?(user_id)
  end

  test "given no user id in the session, uses user_data cookie if present" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{})
      |> put_req_cookie("user_data", ~s/{"name":"Jake Peralta","color":"#000000"}/)
      |> fetch_cookies()
      |> call()

    user_id = get_session(conn, :current_user_id)
    assert %User{name: "Jake Peralta", color: "#000000"} = Users.fetch!(user_id)
  end

  test "given nonexistent user id in the session, generates a new user" do
    conn =
      conn(:get, "/")
      |> init_test_session(%{current_user_id: "no_longer_valid"})
      |> call()

    user_id = get_session(conn, :current_user_id)
    assert user_id != "no_longer_valid"
    assert Users.exists?(user_id)
  end

  test "given existing user id in the session, keeps it" do
    user = User.new()
    Users.save(user)

    conn =
      conn(:get, "/")
      |> init_test_session(%{current_user_id: user.id})
      |> call()

    user_id = get_session(conn, :current_user_id)
    assert user_id == user.id
  end
end
