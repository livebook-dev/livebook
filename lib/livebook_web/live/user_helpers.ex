defmodule LivebookWeb.UserHelpers do
  use Phoenix.Component

  import LivebookWeb.Helpers

  alias Phoenix.LiveView.JS

  @doc """
  Renders user avatar.

  ## Examples

      <.user_avatar user={@user} class="h-20 w-20" text_class="text-3xl" />
  """
  def user_avatar(assigns) do
    assigns =
      assigns
      |> assign_new(:class, fn -> "w-full h-full" end)
      |> assign_new(:text_class, fn -> "" end)

    ~H"""
    <div class={"#{@class} rounded-full flex items-center justify-center"}
      style={"background-color: #{@user.hex_color}"}
      aria-hidden="true">
      <div class={"#{@text_class} text-gray-100 font-semibold"}>
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
  def current_user_modal(assigns) do
    ~H"""
    <.modal id="user-modal" class="w-full max-w-sm">
      <.live_component module={LivebookWeb.UserComponent}
        id="user"
        user={@current_user}
        on_save={hide_current_user_modal()} />
    </.modal>
    """
  end

  def show_current_user_modal(js \\ %JS{}), do: show_modal(js, "user-modal")

  def hide_current_user_modal(js \\ %JS{}), do: hide_modal(js, "user-modal")
end
