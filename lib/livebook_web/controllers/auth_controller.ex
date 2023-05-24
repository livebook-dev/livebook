defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller

  plug(:require_unauthenticated)

  alias LivebookWeb.AuthPlug

  defp require_unauthenticated(conn, _opts) do
    auth_mode = Livebook.Config.auth_mode()

    if auth_mode not in [:password, :token] or AuthPlug.authenticated?(conn, auth_mode) do
      redirect_to(conn)
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
      auth_mode: Livebook.Config.auth_mode(),
      any_public_app?: any_public_app?(),
      empty_apps_path?: Livebook.Apps.empty_apps_path?()
    )
  end

  def authenticate(conn, %{"password" => password}) do
    conn = AuthPlug.store(conn, :password, password)

    if AuthPlug.authenticated?(conn, :password) do
      redirect_to(conn)
    else
      render_form_error(conn, :password)
    end
  end

  def authenticate(conn, %{"token" => token}) do
    conn = AuthPlug.store(conn, :token, token)

    if AuthPlug.authenticated?(conn, :token) do
      redirect_to(conn)
    else
      render_form_error(conn, :token)
    end
  end

  defp render_form_error(conn, auth_mode) do
    errors = [{"%{auth_mode} is invalid", [auth_mode: auth_mode]}]

    render(conn, "index.html",
      errors: errors,
      auth_mode: auth_mode,
      any_public_app?: any_public_app?(),
      empty_apps_path?: Livebook.Apps.empty_apps_path?()
    )
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

  defp any_public_app?() do
    Livebook.Apps.list_apps()
    |> Enum.any?(& &1.public?)
  end
end
