defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller

  plug :require_unauthenticated_password

  alias LivebookWeb.Helpers

  defp require_unauthenticated_password(conn, _opts) do
    if LivebookWeb.AuthPlug.authenticated?(conn) or Livebook.Config.auth_mode() != :password do
      redirect(conn, to: "/")
    else
      conn
    end
  end

  def index(conn, _params) do
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
