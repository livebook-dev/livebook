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
          <button class="text-gray-400 hover:text-current"
            phx-click="delete_session"
            phx-value-id="<%= summary.session_id %>"
            phx-target="<%= @myself %>"
            aria-label="delete">
            <%= remix_icon("delete-bin-line", class: "text-xl") %>
          </button>
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
end
