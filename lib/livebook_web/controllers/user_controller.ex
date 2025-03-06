defmodule LivebookWeb.UserController do
  use LivebookWeb, :controller

  def logout(conn, _params) do
    if get_session(conn, :user_id) do
      {_type, module, _key} = Livebook.Config.identity_provider()

      case module.logout(LivebookWeb.ZTA, conn) do
        :ok ->
          conn
          |> configure_session(renew: true)
          |> clear_session()
          |> render("logout.html")

        {:error, reason} ->
          conn
          |> redirect(to: ~p"/")
          |> put_flash(:error, reason)
      end
    else
      redirect(conn, to: ~p"/")
    end
  end
end
