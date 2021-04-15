defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller

  alias LivebookWeb.Helpers

  def index(conn, _assigns) do
    conn
    |> set_authenticated()
    |> ensure_authenticated()
  end

  defp set_authenticated(conn) do
    conn
    |> assign(:authenticated, LivebookWeb.AuthPlug.authenticated?(conn))
  end

  defp ensure_authenticated(%Plug.Conn{assigns: %{authenticated: true}} = conn) do
    conn
    |> redirect(to: "/")
  end

  defp ensure_authenticated(conn) do
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
