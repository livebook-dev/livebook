defmodule LiveBookWeb.PageController do
  use LiveBookWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
