defmodule LivebookWeb.SessionsComponent do
  use LivebookWeb, :live_component

  alias Livebook.SessionSupervisor

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-col space-y-4">
      <%= for summary <- @session_summaries do %>
        <div class="p-5 flex items-center border border-gray-200 rounded-lg">
          <div class="flex-grow flex flex-col space-y-1">
            <%= live_redirect summary.notebook_name, to: Routes.session_path(@socket, :page, summary.session_id),
              class: "font-semibold text-gray-800 hover:text-gray-900" %>
            <div class="text-gray-600 text-sm">
              <%= summary.path || "No file" %>
            </div>
          </div>
          <div class="relative">
            <button data-element="menu-toggle">
              <%= remix_icon("more-2-fill", class: "text-xl action-icon") %>
            </button>
            <div class="absolute right-0 z-20 rounded-lg shadow-center bg-white flex flex-col py-2" data-element="menu">
              <button class="flex space-x-3 px-5 py-2 items-center text-gray-500 hover:bg-gray-50"
                phx-click="fork_session"
                phx-value-id="<%= summary.session_id %>"
                phx-target="<%= @myself %>">
                <%= remix_icon("git-branch-line") %>
                <span class="font-medium">Fork</span>
              </button>
              <button class="flex space-x-3 px-5 py-2 items-center text-red-600 hover:bg-gray-50"
                phx-click="delete_session"
                phx-value-id="<%= summary.session_id %>"
                phx-target="<%= @myself %>">
                <%= remix_icon("delete-bin-6-line") %>
                <span class="font-medium">Delete</span>
              </button>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("delete_session", %{"id" => session_id}, socket) do
    SessionSupervisor.delete_session(session_id)
    {:noreply, socket}
  end

  def handle_event("fork_session", %{"id" => session_id}, socket) do
    send(self(), {:fork_session, session_id})
    {:noreply, socket}
  end
end
