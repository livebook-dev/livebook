defmodule LivebookWeb.HomeLive.SessionListComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, order_by: "date")}
  end

  @impl true
  def update(assigns, socket) do
    {sessions, assigns} = Map.pop!(assigns, :sessions)

    sessions = sort_sessions(sessions, socket.assigns.order_by)

    socket =
      socket
      |> assign(assigns)
      |> assign(:sessions, sessions)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <div class="flex items-center justify-between">
        <h2 class="mb-4 uppercase font-semibold text-gray-500">
          Running sessions (<%= length(@sessions) %>)
        </h2>
        <div class="relative" id={"sessions-order-menu"} phx-hook="Menu" data-element="menu">
          <button class="button button-outlined-gray px-4 py-1" data-toggle>
            <span><%= order_by_label(@order_by) %></span>
            <.remix_icon icon="arrow-down-s-line" class="text-lg leading-none align-middle ml-1" />
          </button>
          <div class="menu" data-content>
            <%= for order_by <- ["date", "title"] do %>
              <button class={"menu__item #{if order_by == @order_by, do: "text-gray-900", else: "text-gray-500"}"}
                phx-click={JS.push("set_order", value: %{order_by: order_by}, target: @myself)}>
                <.remix_icon icon={order_by_icon(order_by)} />
                <span class="font-medium"><%= order_by_label(order_by) %></span>
              </button>
            <% end %>
          </div>
        </div>
      </div>
      <.session_list sessions={@sessions} socket={@socket} />
    </div>
    """
  end

  defp session_list(%{sessions: []} = assigns) do
    ~H"""
    <div class="p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
      <div>
        <.remix_icon icon="windy-line" class="text-gray-400 text-xl" />
      </div>
      <div class="text-gray-600">
        You do not have any running sessions.
        <br>
        Please create a new one by clicking <span class="font-semibold">“New notebook”</span>
      </div>
    </div>
    """
  end

  defp session_list(assigns) do
    ~H"""
    <div class="flex flex-col">
      <%= for session <- @sessions do %>
        <div class="py-4 flex items-center border-b border-gray-300"
          data-test-session-id={session.id}>
          <div class="flex-grow flex flex-col items-start">
            <%= live_redirect session.notebook_name,
                  to: Routes.session_path(@socket, :page, session.id),
                  class: "font-semibold text-gray-800 hover:text-gray-900" %>
            <div class="text-gray-600 text-sm">
              <%= if session.file, do: session.file.path, else: "No file" %>
            </div>
            <div class="mt-2 text-gray-600 text-sm">
              Created <%= format_creation_date(session.created_at) %>
            </div>
          </div>
          <div class="relative" id={"session-#{session.id}-menu"} phx-hook="Menu" data-element="menu">
            <button class="icon-button" data-toggle>
              <.remix_icon icon="more-2-fill" class="text-xl" />
            </button>
            <div class="menu" data-content>
              <button class="menu__item text-gray-500"
                phx-click="fork_session"
                phx-value-id={session.id}>
                <.remix_icon icon="git-branch-line" />
                <span class="font-medium">Fork</span>
              </button>
              <a class="menu__item text-gray-500"
                href={live_dashboard_process_path(@socket, session.pid)}
                target="_blank">
                <.remix_icon icon="dashboard-2-line" />
                <span class="font-medium">See on Dashboard</span>
              </a>
              <%= live_patch to: Routes.home_path(@socket, :close_session, session.id),
                    class: "menu__item text-red-600" do %>
                <.remix_icon icon="close-circle-line" />
                <span class="font-medium">Close</span>
              <% end %>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("set_order", %{"order_by" => order_by}, socket) do
    sessions = sort_sessions(socket.assigns.sessions, order_by)
    {:noreply, assign(socket, sessions: sessions, order_by: order_by)}
  end

  def format_creation_date(created_at) do
    time_words = created_at |> DateTime.to_naive() |> Livebook.Utils.Time.time_ago_in_words()
    time_words <> " ago"
  end

  defp order_by_label("date"), do: "Date"
  defp order_by_label("title"), do: "Title"

  defp order_by_icon("date"), do: "calendar-2-line"
  defp order_by_icon("title"), do: "text"

  defp sort_sessions(sessions, "date") do
    Enum.sort_by(sessions, & &1.created_at, {:desc, DateTime})
  end

  defp sort_sessions(sessions, "title") do
    Enum.sort_by(sessions, fn session ->
      {session.notebook_name, -DateTime.to_unix(session.created_at)}
    end)
  end
end
