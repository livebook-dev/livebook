defmodule LivebookWeb.AuthHook do
  import Phoenix.LiveView

  alias LivebookWeb.Router.Helpers, as: Routes

  def on_mount(:default, _params, session, socket) do
    uri = get_connect_info(socket, :uri)
    auth_mode = Livebook.Config.auth_mode()

    if LivebookWeb.AuthPlug.authenticated?(session || %{}, uri.port, auth_mode) do
      {:cont, socket}
    else
      {:halt, redirect(socket, to: Routes.home_path(socket, :page))}
    end
  end
end
