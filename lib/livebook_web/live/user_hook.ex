defmodule LivebookWeb.CurrentUserHook do
  import Phoenix.LiveView

  alias Livebook.Users.User

  def on_mount(:default, _params, %{"current_user_id" => current_user_id} = session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "users:#{current_user_id}")
    end

    socket =
      socket
      |> assign_new(:current_user, fn -> build_current_user(session, socket) end)
      |> attach_hook(:current_user_subscription, :handle_info, &info/2)

    {:cont, socket}
  end

  defp info(
         {:user_change, %{id: id} = user},
         %{assigns: %{current_user: %{id: id}}} = socket
       ) do
    {:cont, assign(socket, :current_user, user)}
  end

  defp info(_message, socket), do: {:cont, socket}

  # Builds `Livebook.Users.User` using information from
  # session and socket.
  #
  # Uses `user_data` from socket `connect_params` as initial
  # attributes if the socket is connected. Otherwise uses
  # `user_data` from session.
  defp build_current_user(session, socket) do
    %{"current_user_id" => current_user_id} = session

    connect_params = get_connect_params(socket) || %{}
    user_data = connect_params["user_data"] || session["user_data"] || %{}

    case User.change(%{User.new() | id: current_user_id}, user_data) do
      {:ok, user} -> user
      {:error, _errors, user} -> user
    end
  end
end
