defmodule LivebookWeb.NotebookCardComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-col">
      <div class="flex items-center justify-center p-6 border-2 border-gray-100 rounded-t-2xl h-[150px]">
        <img src="<%= @notebook_info.image_url %>" class="max-h-full max-w-[75%]" />
      </div>
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
