defmodule LivebookWeb.LearnHelpers do
  use Phoenix.Component

  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders an learn notebook card.
  """
  def notebook_card(assigns) do
    ~H"""
    <%= live_redirect to: Routes.learn_path(@socket, :notebook, @notebook_info.slug),
            class: "flex flex-col border-2 border-gray-100 hover:border-gray-200 rounded-2xl" do %>
      <div class="flex items-center justify-center p-6 border-b-2 border-gray-100 rounded-t-2xl h-[150px]">
        <img
          src={Path.join(Livebook.Config.base_url_path(), @notebook_info.details.cover_url)}
          class="max-h-full max-w-[75%]"
          alt={"#{@notebook_info.title} logo"}
        />
      </div>
      <div class="px-6 py-4 bg-gray-100 rounded-b-2xl grow">
        <span class="text-gray-800 font-semibold"><%= @notebook_info.title %></span>
        <p class="mt-2 text-sm text-gray-600">
          <%= @notebook_info.details.description %>
        </p>
      </div>
    <% end %>
    """
  end
end
