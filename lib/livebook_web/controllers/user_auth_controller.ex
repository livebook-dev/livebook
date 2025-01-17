defmodule LivebookWeb.UserAuthController do
  use LivebookWeb, :controller
  alias LivebookWeb.AuthHelpers

  def logout(conn, _params) do
    if get_session(conn, :user_id) do
      conn
      |> configure_session(renew: true)
      |> clear_session()
      |> render("logout.html")
    else
      AuthHelpers.redirect_to(conn)
    end
  end
end
