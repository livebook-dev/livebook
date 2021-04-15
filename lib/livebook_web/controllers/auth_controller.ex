defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller

  alias LivebookWeb.Helpers

  def index(conn, assigns) do
    conn
    |> put_view(LivebookWeb.ErrorView)
    |> render("401_secret.html", Map.put(assigns, :type, :password))
  end

  def authenticate(conn, %{"password" => password}) do
    cookie_key = Helpers.auth_cookie_key(conn, :password)

    conn
    |> put_resp_cookie(cookie_key, password, Helpers.auth_cookie_opts())
    |> redirect(to: "/")
  end
end
