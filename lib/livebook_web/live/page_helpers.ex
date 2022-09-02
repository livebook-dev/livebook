defmodule LivebookWeb.PageHelpers do
  use Phoenix.Component

  @doc """
  Renders page title.

  ## Examples

      <.title text="Explore" socket={@socket} />
  """
  def title(assigns) do
    ~H"""
    <h1 class="text-2xl text-gray-800 font-medium">
      <%= @text %>
    </h1>
    """
  end
end
