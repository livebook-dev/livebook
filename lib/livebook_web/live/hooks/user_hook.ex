defmodule LivebookWeb.UserHook do
  import Phoenix.Component
  import Phoenix.LiveView

  def on_mount(:default, _params, session, socket) do
    socket =
      socket
      |> assign_new(:current_user, fn ->
        connect_params = get_connect_params(socket) || %{}
        identity_data = session["identity_data"]
        # user_data from connect params takes precedence, since the
        # cookie may have been altered by the client.
        user_data = connect_params["user_data"] || session["user_data"]
        LivebookWeb.UserPlug.build_current_user(session, identity_data, user_data)
      end)
      |> attach_hook(:current_user_subscription, :handle_info, &info/2)

    if connected?(socket) do
      Livebook.Users.subscribe(socket.assigns.current_user.id)
    end

    Logger.metadata(Livebook.Utils.logger_users_metadata([socket.assigns.current_user]))

    {:cont, socket}
  end

  defp info(
         {:user_change, %{id: id} = user},
         %{assigns: %{current_user: %{id: id}}} = socket
       ) do
    {:halt, assign(socket, :current_user, user)}
  end

  defp info(_message, socket), do: {:cont, socket}
end
