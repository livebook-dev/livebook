defmodule LivebookWeb.PageHelpers do
  use Phoenix.Component

  import LivebookWeb.LiveHelpers

  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders page title.

  ## Examples

      <.title text="Explore" socket={@socket} />
  """
  def title(assigns) do
    ~H"""
    <h1 class="text-3xl text-gray-800 font-semibold">
      <%= @text %>
    </h1>
    """
  end
end
