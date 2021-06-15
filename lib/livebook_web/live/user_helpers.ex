defmodule LivebookWeb.UserHelpers do
  import Phoenix.LiveView
  import Phoenix.LiveView.Helpers

  alias Livebook.Users.User

  @doc """
  Renders user avatar,

  ## Options

    * `:class` - class added to the avatar box

    * `:text_class` - class added to the avatar text
  """
  def render_user_avatar(user, opts \\ []) do
    assigns = %{
      name: user.name,
      hex_color: user.hex_color,
      class: Keyword.get(opts, :class, "w-full h-full"),
      text_class: Keyword.get(opts, :text_class)
    }

    ~L"""
    <div class="rounded-full <%= @class %> flex items-center justify-center" style="background-color: <%= @hex_color %>">
      <div class="<%= @text_class %> text-gray-100 font-semibold">
        <%= avatar_text(@name) %>
      </div>
    </div>
    """
  end

  defp avatar_text(nil), do: "?"

  defp avatar_text(name) do
    name
    |> String.split()
    |> Enum.map(&String.at(&1, 0))
    |> Enum.map(&String.upcase/1)
    |> case do
      [initial] -> initial
      initials -> List.first(initials) <> List.last(initials)
    end
  end

  @doc """
  Builds `Livebook.Users.User` using information from
  session and socket.

  Uses `user_data` from socket `connect_params` as initial
  attributes if the socket is connected. Otherwise uses
  `user_data` from session.
  """
  def build_current_user(session, socket) do
    %{"current_user_id" => current_user_id} = session

    connect_params = get_connect_params(socket) || %{}
    user_data = connect_params["user_data"] || session["user_data"] || %{}

    case User.change(%{User.new() | id: current_user_id}, user_data) do
      {:ok, user} -> user
      {:error, _errors, user} -> user
    end
  end
end
