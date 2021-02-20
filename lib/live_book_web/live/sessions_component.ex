defmodule LiveBookWeb.SessionsComponent do
  use LiveBookWeb, :live_component

  alias LiveBook.SessionSupervisor

  @impl true
  def render(assigns) do
    ~L"""
    <div class="mt-3 flex flex-col space-y-2">
      <%= for summary <- @session_summaries do %>
        <div class="shadow rounded-md p-2">
          <div class="p-3 flex items-center">
            <div class="flex-grow flex flex-col space-y-1 text-gray-700 text-lg hover:text-gray-900">
              <%= live_redirect summary.notebook_name, to: Routes.session_path(@socket, :page, summary.session_id) %>
              <div class="text-gray-500 text-sm">
                <%= summary.path || "No file" %>
              </div>
            </div>
            <button class="text-gray-500 hover:text-current"
              phx-click="delete_session"
              phx-value-id="<%= summary.session_id %>"
              phx-target="<%= @myself %>"
              aria-label="delete">
              <%= Icons.svg(:trash, class: "h-6") %>
            </button>
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
end
