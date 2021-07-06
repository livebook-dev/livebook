defmodule LivebookWeb.ExploreHelpers do
  use Phoenix.Component

  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders an explore notebook card.
  """
  def notebook_card(assigns) do
    ~H"""
    <div class="flex flex-col">
      <%= live_redirect to: Routes.explore_path(@socket, :notebook, @notebook_info.slug),
            class: "flex items-center justify-center p-6 border-2 border-gray-100 rounded-t-2xl h-[150px]" do %>
        <img src={@notebook_info.image_url} class="max-h-full max-w-[75%]" />
      <% end %>
      <div class="px-6 py-4 bg-gray-100 rounded-b-2xl flex-grow">
        <%= live_redirect @notebook_info.title,
              to: Routes.explore_path(@socket, :notebook, @notebook_info.slug),
              class: "text-gray-800 font-semibold cursor-pointer" %>
        <p class="mt-2 text-sm text-gray-600">
          <%= @notebook_info.description %>
        </p>
      </div>
    </div>
    """
  end
end
