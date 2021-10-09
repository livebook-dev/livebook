defmodule LivebookWeb.PageHelpers do
  use Phoenix.Component

  import LivebookWeb.Helpers

  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders page title.

  ## Examples

      <.title text="Explore" socket={@socket} />
  """
  def title(assigns) do
    ~H"""
    <div class="relative">
      <%= live_patch to: Routes.home_path(@socket, :page),
            class: "hidden md:block absolute top-[50%] left-[-12px] transform -translate-y-1/2 -translate-x-full" do %>
        <.remix_icon icon="arrow-left-line" class="text-2xl align-middle" />
      <% end %>
      <h1 class="text-3xl text-gray-800 font-semibold">
        <%= @text %>
      </h1>
    </div>
    """
  end
end
