defmodule LivebookWeb.AppAuthHook do
  import Phoenix.Component
  import Phoenix.LiveView

  use LivebookWeb, :verified_routes

  # For apps with password, we want to store the hashed password
  # (let's call it token) in the session, as we do for the main auth.
  # However, the session uses cookies, which have a ~4kb size limit.
  # We could use multiple cookies, however there are also limits on
  # the number of cookies, and we don't want the browser to clear
  # them all at some point. Additionally, accumulating cookies for
  # all apps would imply larger payloads on every regular request.
  #
  # Since we don't have any persistence on the server side, the other
  # option is to use the browser local storage and manage it through
  # JavaScript. Therefore, the whole auth is built using LiveView.
  # The flows are:
  #
  #   * unauthenticated - the auth LiveView shows a regular form and
  #     validates the user-provided password. Once the password is
  #     correct, it pushes an event to the client to store the token.
  #     Once the token is stored it redirects to the app page.
  #
  #   * authenticated - on dead render the app LiveView renders just
  #     a loading screen. On the client side, provided it's the app
  #     page, we read the token from local storage (if stored) and
  #     send it in mount connect params via the socket. Then on the
  #     server we use that token to authenticate.
  #
  # This module defines a hook that sets the following assigns:
  #
  #   * `:app_authenticated?` - reflects the current authentication.
  #     For public apps (or in case the user has full access) it is
  #     set to `true` on both dead and live render
  #
  #   * `:livebook_authenticated?` - if the user has full Livebook
  #     access
  #
  #   * `:app_settings` - the current app settings
  #

  def on_mount(:default, %{"slug" => slug}, session, socket) do
    livebook_authenticated? = livebook_authenticated?(session, socket)

    socket = assign(socket, livebook_authenticated?: livebook_authenticated?)

    case Livebook.Apps.fetch_settings(slug) do
      {:ok, %{access_type: :public} = app_settings} ->
        {:cont, assign(socket, app_authenticated?: true, app_settings: app_settings)}

      {:ok, %{access_type: :protected} = app_settings} ->
        app_authenticated? = livebook_authenticated? or has_valid_token?(socket, app_settings)

        {:cont,
         assign(socket, app_authenticated?: app_authenticated?, app_settings: app_settings)}

      :error ->
        {:halt, redirect(socket, to: ~p"/")}
    end
  end

  defp livebook_authenticated?(session, socket) do
    uri = get_connect_info(socket, :uri)
    LivebookWeb.AuthPlug.authenticated?(session, uri.port, Livebook.Config.auth_mode())
  end

  defp has_valid_token?(socket, app_settings) do
    connect_params = get_connect_params(socket) || %{}

    if token = connect_params["app_auth_token"] do
      valid_auth_token?(token, app_settings)
    else
      false
    end
  end

  @doc """
  Generates auth token that can be sent to the client.
  """
  @spec get_auth_token(Livebook.Notebook.AppSettings.t()) :: String.t()
  def get_auth_token(app_settings) do
    :crypto.hash(:sha256, app_settings.password) |> Base.encode64()
  end

  @doc """
  Checks the given token is valid.
  """
  @spec valid_auth_token?(String.t(), Livebook.Notebook.AppSettings.t()) :: boolean()
  def valid_auth_token?(token, app_settings) do
    Plug.Crypto.secure_compare(token, get_auth_token(app_settings))
  end

  @doc """
  Checks if the given password is valid.
  """
  @spec valid_password?(String.t(), Livebook.Notebook.AppSettings.t()) :: boolean()
  def valid_password?(password, app_settings) do
    Plug.Crypto.secure_compare(password, app_settings.password)
  end
end
