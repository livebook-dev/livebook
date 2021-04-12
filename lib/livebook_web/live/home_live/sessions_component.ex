defmodule LivebookWeb.SessionLive.SessionsComponent do
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
          <div class="relative">
            <button class="icon-button" data-element="menu-toggle">
              <%= remix_icon("more-2-fill", class: "text-xl") %>
            </button>
            <div class="absolute right-0 z-20 rounded-lg shadow-center bg-white flex flex-col py-2" data-element="menu">
              <button class="flex space-x-3 px-5 py-2 items-center text-gray-500 hover:bg-gray-50"
                phx-click="fork_session"
                phx-value-id="<%= summary.session_id %>">
                <%= remix_icon("git-branch-line") %>
                <span class="font-medium">Fork</span>
              </button>
              <%= live_patch to: Routes.home_path(@socket, :close_session, summary.session_id),
                    class: "flex space-x-3 px-5 py-2 items-center text-red-600 hover:bg-gray-50" do %>
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
