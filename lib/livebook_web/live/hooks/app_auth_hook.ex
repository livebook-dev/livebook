defmodule LivebookWeb.AppAuthHook do
  import Phoenix.LiveView

  def on_mount(:default, %{"slug" => slug}, session, socket) do
    case Livebook.Apps.fetch_settings_by_slug(slug) do
      {:ok, %{access_type: :public}} ->
        {:cont, socket}

      {:ok, %{access_type: :protected} = app_settings} ->
        uri = get_connect_info(socket, :uri)

        if LivebookWeb.AppAuthPlug.authenticated?(session || %{}, uri.port, app_settings) do
          {:cont, socket}
        else
          {:halt, socket}
        end

      :error ->
        {:halt, socket}
    end
  end
end
