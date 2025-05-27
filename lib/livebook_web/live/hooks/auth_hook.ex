defmodule LivebookWeb.AuthHook do
  import Phoenix.LiveView

  use LivebookWeb, :verified_routes

  def on_mount(:default, _params, session, socket) do
    uri = get_connect_info(socket, :uri)
    socket = attach_hook(socket, :authorization_subscription, :handle_info, &handle_info/2)

    if LivebookWeb.AuthPlug.authorized?(session || %{}, uri.port) do
      {:cont, socket}
    else
      {:halt, redirect(socket, to: ~p"/")}
    end
  end

  defp handle_info({:server_authorization_updated, %{hub_id: hub_id}}, socket) do
    # We already updated the current user, so we just need to force the redirection.
    # But, for apps, we redirect directly from the app session
    current_user = socket.assigns.current_user

    if current_user.payload do
      if current_user.access_type == :full and
           Livebook.Hubs.TeamClient.user_full_access?(hub_id, current_user.groups) do
        {:halt, socket}
      else
        {:halt, redirect(socket, to: ~p"/")}
      end
    else
      {:halt, socket}
    end
  end

  defp handle_info(_message, socket), do: {:cont, socket}
end
