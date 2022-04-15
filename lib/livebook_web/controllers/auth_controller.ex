defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller

  plug :require_unauthenticated

  alias LivebookWeb.AuthPlug

  defp require_unauthenticated(conn, _opts) do
    auth_mode = Livebook.Config.auth_mode()

    if auth_mode not in [:password, :token] or AuthPlug.authenticated?(conn, auth_mode) do
      redirect_to(conn)
    else
      conn
    end
  end

  def index(conn, _params) do
    render(conn, "index.html", auth_mode: Livebook.Config.auth_mode())
  end

  def authenticate(conn, %{"password" => password}) do
    conn = AuthPlug.store(conn, :password, password)

    if AuthPlug.authenticated?(conn, :password) do
      redirect_to(conn)
    else
      index(conn, %{})
    end
  end

  def authenticate(conn, %{"token" => token}) do
    conn = AuthPlug.store(conn, :token, token)

    if AuthPlug.authenticated?(conn, :token) do
      redirect_to(conn)
    else
      index(conn, %{})
    end
  end

  defp redirect_to(conn) do
    conn
    |> then(fn conn ->
      if redirect_to = get_session(conn, :redirect_to) do
        conn
        |> delete_session(:redirect_to)
        |> redirect(to: redirect_to)
      else
        redirect(conn, to: "/")
      end
    end)
    |> halt()
  end
end
