defmodule LivebookWeb.NotebookCardComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.SessionHelpers

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-col">
      <div class="flex items-center justify-center p-6 border-2 border-gray-100 rounded-t-2xl h-[150px]">
        <img src="<%= @image_url %>" class="max-h-full max-w-[75%]" />
      </div>
      <div class="px-6 py-4 bg-gray-100 rounded-b-2xl flex-grow">
        <h3 class="text-gray-800 font-semibold cursor-pointer"
          phx-click="fork"
          phx-target="<%= @myself %>">
          <%= @notebook.name %>
        </h3>
        <p class="mt-2 text-sm text-gray-600">
          <%= @description %>
        </p>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("fork", %{}, socket) do
    {:noreply, create_session(socket, notebook: socket.assigns.notebook)}
  end
end
