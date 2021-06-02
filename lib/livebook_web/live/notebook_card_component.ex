defmodule LivebookWeb.NotebookCardComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.SessionHelpers

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 rounded-2xl bg-gray-100">
      <h3 class="text-xl text-gray-800 font-semibold cursor-pointer"
        phx-click="fork"
        phx-target="<%= @myself %>">
        <%= @notebook.name %>
      </h3>
      <p class="mt-2 text-sm text-gray-500">
        <%= @description %>
      </p>
    </div>
    """
  end

  @impl true
  def handle_event("fork", %{}, socket) do
    {:noreply, create_session(socket, notebook: socket.assigns.notebook)}
  end
end
