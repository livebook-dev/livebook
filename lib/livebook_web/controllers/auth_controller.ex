defmodule LivebookWeb.AuthController do
  use LivebookWeb, :controller

  def index(conn, assigns) do
    conn
    |> put_view(LivebookWeb.ErrorView)
    |> render("401_secret.html", Map.put(assigns, :type, :password))
  end

  def authenticate(conn, %{"secret" => _password}) do
    # on success redirect to home path
    LivebookWeb.AuthPlug.call(conn, path: "/")
  end
end
