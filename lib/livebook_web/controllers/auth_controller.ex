defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller

  plug :require_unauthenticated_password

  alias LivebookWeb.AuthPlug

  defp require_unauthenticated_password(conn, _opts) do
    if Livebook.Config.auth_mode() != :password or AuthPlug.authenticated?(conn, :password) do
      redirect_home(conn)
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
    conn = AuthPlug.store(conn, :password, password)

    if AuthPlug.authenticated?(conn, :password) do
      redirect_home(conn)
    else
      index(conn, %{})
    end
  end

  defp redirect_home(conn) do
    conn
    |> redirect(to: "/")
    |> halt()
  end
end
