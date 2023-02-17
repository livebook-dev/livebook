defmodule LivebookWeb.AppAuthController do
  use LivebookWeb, :controller

  plug :require_unauthenticated

  alias LivebookWeb.Router.Helpers, as: Routes
  alias LivebookWeb.AppAuthPlug

  defp require_unauthenticated(conn, _opts) do
    if AppAuthPlug.authenticated?(conn) do
      redirect_to_app(conn)
    else
      conn
    end
  end

  def index(conn, params) do
    slug = conn.path_params["slug"]

    render(conn, "index.html", errors: params["errors"], slug: slug)
  end

  def authenticate(conn, %{"password" => password}) do
    conn = AppAuthPlug.store(conn, password)

    if AppAuthPlug.authenticated?(conn) do
      redirect_to_app(conn)
    else
      index(conn, %{"errors" => [{"app password is invalid", []}]})
    end
  end

  def root(conn, _params) do
    slug = conn.path_params["slug"]

    conn
    |> put_session(:redirect_to, Routes.app_path(conn, :page, slug))
    |> redirect(to: Routes.path(conn, "/authenticate"))
    |> halt()
  end

  defp redirect_to_app(conn) do
    slug = conn.path_params["slug"]

    conn
    |> redirect(to: Routes.app_path(conn, :page, slug))
    |> halt()
  end
end
