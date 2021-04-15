defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller

  alias LivebookWeb.Helpers

  def index(conn, assigns) do
    conn
    |> put_view(LivebookWeb.ErrorView)
    |> render("401.html")
  end

  def authenticate(conn, %{"password" => password}) do
    password = :crypto.hash(:sha256, password) |> Base.encode16()
    cookie_key = Helpers.auth_cookie_key(conn, :password)

    conn
    |> put_resp_cookie(cookie_key, password, Helpers.auth_cookie_opts())
    |> redirect(to: "/")
  end
end
