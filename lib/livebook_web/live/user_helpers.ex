defmodule LivebookWeb.UserHelpers do
  use LivebookWeb, :html

  @doc """
  Renders user avatar.

  ## Examples

      <.user_avatar user={@user} class="h-20 w-20" text_class="text-3xl" />

  """
  attr :user, Livebook.Users.User, required: true
  attr :class, :string, default: "w-full h-full"
  attr :text_class, :string, default: nil

  def user_avatar(assigns) do
    ~H"""
    <div
      class={["rounded-full flex items-center justify-center", @class]}
      style={"background-color: #{@user.hex_color}"}
      aria-hidden="true"
    >
      <div class={["text-gray-100 font-semibold", @text_class]}>
        <%= avatar_text(@user.name) %>
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
  Renders the current user edit form in a modal.

  ## Examples

      <.current_user_modal current_user={@current_user} />

  """
  attr :current_user, Livebook.Users.User, required: true

  def current_user_modal(assigns) do
    ~H"""
    <.modal id="user-modal" width={:small}>
      <.live_component
        module={LivebookWeb.UserComponent}
        id="user"
        user={@current_user}
        on_save={hide_current_user_modal()}
      />
    </.modal>
    """
  end

  def show_current_user_modal(js \\ %JS{}), do: show_modal(js, "user-modal")

  def hide_current_user_modal(js \\ %JS{}), do: hide_modal(js, "user-modal")
end
