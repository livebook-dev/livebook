defmodule LivebookWeb.UserHook do
  use LivebookWeb, :verified_routes

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
      |> attach_hook(:current_user_subscription, :handle_info, &handle_info/2)
      |> attach_hook(:server_authorization_subscription, :handle_info, &handle_info/2)

    if connected?(socket) do
      Livebook.Users.subscribe(socket.assigns.current_user.id)
      Livebook.Teams.Broadcasts.subscribe(:app_server)
    end

    Logger.metadata(Livebook.Utils.logger_users_metadata([socket.assigns.current_user]))

    {:cont, socket}
  end

  defp handle_info(
         {:user_change, %{id: id} = user},
         %{assigns: %{current_user: %{id: id}}} = socket
       ) do
    {:halt, assign(socket, :current_user, user)}
  end

  defp handle_info({:server_authorization_updated, deployment_group}, socket) do
    # Since we checks if the updated deployment group we received belongs
    # to the current app server, we don't need to check here.
    current_user = socket.assigns.current_user
    hub_id = deployment_group.hub_id

    if current_user.payload do
      metadata = Livebook.ZTA.LivebookTeams.build_metadata(hub_id, current_user.payload)

      case Livebook.Users.update_user(current_user, metadata) do
        {:ok, user} -> {:cont, assign(socket, :current_user, user)}
        _otherwise -> {:cont, socket}
      end
    else
      {:cont, socket}
    end
  end

  defp handle_info(_message, socket), do: {:cont, socket}
end
