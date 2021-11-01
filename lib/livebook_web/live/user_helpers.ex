defmodule LivebookWeb.UserHelpers do
  use Phoenix.Component

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
end
