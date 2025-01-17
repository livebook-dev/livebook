defmodule LivebookWeb.AuthHelpers do
  use LivebookWeb, :controller

  def redirect_to(conn) do
    conn
    |> then(fn conn ->
      if redirect_to = get_session(conn, :redirect_to) do
        conn
        |> delete_session(:redirect_to)
        |> redirect(to: redirect_to)
      else
        redirect(conn, to: ~p"/")
      end
    end)
    |> halt()
  end
end
