defmodule LivebookWeb.ErrorController do
  use LivebookWeb, :controller

  def show(conn, %{"error" => "unauthorized"}) do
    conn
    |> put_status(:unauthorized)
    |> render("401.html", %{status: 401})
  end
end
