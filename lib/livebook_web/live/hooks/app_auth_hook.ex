defmodule LivebookWeb.AppAuthHook do
  import Phoenix.Component
  import Phoenix.LiveView

  alias LivebookWeb.Router.Helpers, as: Routes

  def on_mount(:default, %{"slug" => slug}, session, socket) do
    case Livebook.Apps.fetch_settings_by_slug(slug) do
      {:ok, %{access_type: :public}} ->
        {:cont, assign(socket, app_authenticated?: true)}

      {:ok, %{access_type: :protected} = app_settings} ->
        cond do
          livebook_authenticated?(session, socket) ->
            {:cont, assign(socket, app_authenticated?: true)}

          connected?(socket) ->
            if has_valid_token?(socket, app_settings) do
              {:cont, assign(socket, app_authenticated?: true)}
            else
              {:halt, push_navigate(socket, to: Routes.app_auth_path(socket, :page, slug))}
            end

          true ->
            {:cont, assign(socket, app_authenticated?: false)}
        end

      :error ->
        {:halt, socket}
    end
  end

  defp livebook_authenticated?(session, socket) do
    uri = get_connect_info(socket, :uri)
    LivebookWeb.AuthPlug.authenticated?(session, uri.port, Livebook.Config.auth_mode())
  end

  defp has_valid_token?(socket, app_settings) do
    connect_params = get_connect_params(socket)

    if token = connect_params["app_auth_token"] do
      password_hash = Base.decode64!(token)
      reference_hash = :crypto.hash(:sha256, app_settings.password)
      Plug.Crypto.secure_compare(password_hash, reference_hash)
    else
      false
    end
  end
end
