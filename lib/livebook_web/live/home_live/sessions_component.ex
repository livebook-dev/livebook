defmodule LivebookWeb.HomeLive.SessionsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-col space-y-4">
      <%= for summary <- @session_summaries do %>
        <div class="p-5 flex items-center border border-gray-200 rounded-lg"
          data-test-session-id="<%= summary.session_id %>">
          <div class="flex-grow flex flex-col space-y-1">
            <%= live_redirect summary.notebook_name, to: Routes.session_path(@socket, :page, summary.session_id),
              class: "font-semibold text-gray-800 hover:text-gray-900" %>
            <div class="text-gray-600 text-sm">
              <%= summary.path || "No file" %>
            </div>
          </div>
          <div class="relative" id="session-<%= summary.session_id %>-menu" phx-hook="Menu" data-element="menu">
            <button class="icon-button" data-toggle>
              <%= remix_icon("more-2-fill", class: "text-xl") %>
            </button>
            <div class="menu" data-content>
              <button class="menu__item text-gray-500"
                phx-click="fork_session"
                phx-value-id="<%= summary.session_id %>">
                <%= remix_icon("git-branch-line") %>
                <span class="font-medium">Fork</span>
              </button>
              <%= link to: live_dashboard_process_path(@socket, summary.pid),
                    class: "menu__item text-gray-500",
                    target: "_blank" do %>
                <%= remix_icon("dashboard-2-line") %>
                <span class="font-medium">See on Dashboard</span>
              <% end %>
              <%= live_patch to: Routes.home_path(@socket, :close_session, summary.session_id),
                    class: "menu__item text-red-600" do %>
                <%= remix_icon("close-circle-line") %>
                <span class="font-medium">Close</span>
              <% end %>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end
end
