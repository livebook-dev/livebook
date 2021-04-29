defmodule LivebookWeb.LiveHelpers do
  import Phoenix.LiveView.Helpers

  @doc """
  Renders user avatar,

  ## Options

    * `:class` - class added to the avatar box

    * `:text_class` - class added to the avatar text
  """
  def render_user_avatar(name, color, opts \\ []) do
    assigns = %{
      name: name,
      color: color,
      class: Keyword.get(opts, :class, "w-full h-full"),
      text_class: Keyword.get(opts, :text_class)
    }

    ~L"""
    <div class="rounded-full <%= @class %> flex items-center justify-center" style="background-color: <%= @color %>">
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
end
