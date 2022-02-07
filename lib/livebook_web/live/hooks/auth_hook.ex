defmodule LivebookWeb.AuthHook do
  import Phoenix.LiveView

  def on_mount(:default, _params, session, socket) do
    uri = get_connect_info(socket, :uri)
    auth_mode = Livebook.Config.auth_mode()

    if LivebookWeb.AuthPlug.authenticated?(session || %{}, uri.port, auth_mode) do
      {:cont, socket}
    else
      {:halt, socket}
    end
  end
end
