defmodule LivebookWeb.UserController do
  use LivebookWeb, :controller

  def logout(conn, _params) do
    if get_session(conn, :user_id) do
      if Livebook.Config.logout_enabled?() do
        do_zta_logout(conn)
      else
        do_logout(conn)
      end
    else
      redirect(conn, to: ~p"/")
    end
  end

  defp do_logout(conn) do
    conn
    |> configure_session(renew: true)
    |> clear_session()
    |> redirect(to: ~p"/")
  end

  defp do_zta_logout(conn) do
    {_type, module, _key} = Livebook.Config.identity_provider()
    module.logout(LivebookWeb.ZTA, conn)
  end
end
