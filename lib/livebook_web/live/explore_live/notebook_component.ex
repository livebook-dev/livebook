defmodule LivebookWeb.ExploreLive.NotebookComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="p-5 flex items-center border border-gray-200 rounded-lg">
        <div class="flex-grow flex flex-col space-y1">
            <div class="font-semibold text-gray-800 hover:text-gray-900"
                phx-click="open"
                phx-value-path="<%= @notebook.path %>"
                phx-value-type="<%= @notebook.type %>">
                <%= @notebook.title %>
            </div>
            <div class="text-gray-600 text-sm" ><%= @notebook.snippet %>...</div>
        </div>
    </div>
    """
  end
end
