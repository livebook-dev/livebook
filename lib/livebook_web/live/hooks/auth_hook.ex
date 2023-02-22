defmodule LivebookWeb.AuthHook do
  import Phoenix.LiveView

  use LivebookWeb, :verified_routes

  def on_mount(:default, _params, session, socket) do
    uri = get_connect_info(socket, :uri)
    auth_mode = Livebook.Config.auth_mode()

    if LivebookWeb.AuthPlug.authenticated?(session || %{}, uri.port, auth_mode) do
      {:cont, socket}
    else
      {:halt, redirect(socket, to: ~p"/")}
    end
  end
end
