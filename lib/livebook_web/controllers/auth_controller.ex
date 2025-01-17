defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller
  alias LivebookWeb.AuthHelpers

  plug(:require_unauthenticated)

  alias LivebookWeb.AuthPlug

  defp require_unauthenticated(conn, _opts) do
    if AuthPlug.authenticated?(conn) do
      AuthHelpers.redirect_to(conn)
    else
      conn
    end
  end

  def index(conn, %{"redirect_to" => path}) do
    conn
    |> put_session(:redirect_to, path)
    |> redirect(to: current_path(conn, %{}))
  end

  def index(conn, _params) do
    render(conn, "index.html",
      errors: [],
      authentication_mode: LivebookWeb.AuthPlug.authentication(conn).mode
    )
  end

  def authenticate(conn, %{"password" => password}) do
    conn = AuthPlug.store(conn, :password, password)

    if AuthPlug.authenticated?(conn) do
      AuthHelpers.redirect_to(conn)
    else
      render_form_error(conn, :password)
    end
  end

  def authenticate(conn, %{"token" => token}) do
    conn = AuthPlug.store(conn, :token, token)

    if AuthPlug.authenticated?(conn) do
      AuthHelpers.redirect_to(conn)
    else
      render_form_error(conn, :token)
    end
  end

  defp render_form_error(conn, authentication_mode) do
    errors = [{"%{authentication_mode} is invalid", [authentication_mode: authentication_mode]}]

    render(conn, "index.html",
      errors: errors,
      authentication_mode: authentication_mode
    )
  end
end
