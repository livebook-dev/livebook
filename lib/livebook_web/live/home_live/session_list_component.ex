defmodule LivebookWeb.HomeLive.SessionListComponent do
  use LivebookWeb, :live_component

  import Livebook.Utils, only: [format_bytes: 1]
  import LivebookWeb.SessionHelpers, only: [uses_memory?: 1]

  @impl true
  def mount(socket) do
    {:ok, assign(socket, order_by: "date"), temporary_assigns: [memory: nil]}
  end

  @impl true
  def update(assigns, socket) do
    {sessions, assigns} = Map.pop!(assigns, :sessions)

    sessions = sort_sessions(sessions, socket.assigns.order_by)

    show_autosave_note? =
      case Livebook.Config.autosave_path() do
        nil -> false
        path -> File.ls!(path) != []
      end

    socket =
      socket
      |> assign(assigns)
      |> assign(
        sessions: sessions,
        show_autosave_note?: show_autosave_note?,
        memory: Livebook.SystemResources.memory()
      )

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <form id="bulk-action-form" phx-submit="bulk_action">
      <div class="mb-4 flex items-center md:items-end justify-between">
        <div class="flex flex-row">
          <h2 class="uppercase font-semibold text-gray-500 text-sm md:text-base">
            Running sessions (<%= length(@sessions) %>)
          </h2>
        </div>
        <div class="flex flex-row">
          <.memory_info memory={@memory} />
          <%= if @sessions != [] do %>
            <.edit_sessions sessions={@sessions} socket={@socket}/>
          <% end %>
          <.menu id="sessions-order-menu">
            <:toggle>
              <button class="w-28 button-base button-outlined-gray px-4 py-1 flex justify-between items-center"
                type="button">
                <span><%= order_by_label(@order_by) %></span>
                <.remix_icon icon="arrow-down-s-line" class="text-lg leading-none align-middle ml-1" />
              </button>
            </:toggle>
            <:content>
              <%= for order_by <- ["date", "title", "memory"] do %>
                <button class={"menu-item #{if order_by == @order_by, do: "text-gray-900", else: "text-gray-500"}"}
                  type="button"
                  role="menuitem"
                  phx-click={JS.push("set_order", value: %{order_by: order_by}, target: @myself)}>
                  <.remix_icon icon={order_by_icon(order_by)} />
                  <span class="font-medium"><%= order_by_label(order_by) %></span>
                </button>
              <% end %>
            </:content>
          </.menu>
        </div>
      </div>
      <.session_list sessions={@sessions} socket={@socket}
        show_autosave_note?={@show_autosave_note?} />
    </form>
    """
  end

  defp session_list(%{sessions: []} = assigns) do
    ~H"""
    <div class="mt-4 p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
      <div>
        <.remix_icon icon="windy-line" class="text-gray-400 text-xl" />
      </div>
      <div class="grow flex items-center justify-between">
        <div class="text-gray-600">
          You do not have any running sessions.
          <%= if @show_autosave_note? do %>
            <br>
            Looking for unsaved notebooks?
            <a class="font-semibold" href="#" phx-click="open_autosave_directory">Browse them here</a>.
          <% end %>
        </div>
        <button class="button-base button-blue" phx-click="new">
          New notebook
        </button>
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
          <div id={"#{session.id}-checkbox"} phx-update="ignore">
            <input type="checkbox" name="session_ids[]" value={session.id}
              class="checkbox-base hidden mr-3"
              data-element="bulk-edit-member"
              phx-click={JS.dispatch("lb:session_list:on_selection_change")}>
          </div>
          <div class="grow flex flex-col items-start">
            <%= live_redirect session.notebook_name,
                  to: Routes.session_path(@socket, :page, session.id),
                  class: "font-semibold text-gray-800 hover:text-gray-900" %>
            <div class="text-gray-600 text-sm">
              <%= if session.file, do: session.file.path, else: "No file" %>
            </div>
            <div class="mt-2 text-gray-600 text-sm flex flex-row items-center">
            <%= if uses_memory?(session.memory_usage) do %>
              <div class="h-3 w-3 mr-1 rounded-full bg-green-500"></div>
              <span class="pr-4"><%= format_bytes(session.memory_usage.runtime.total) %></span>
            <% else %>
              <div class="h-3 w-3 mr-1 rounded-full bg-gray-300"></div>
              <span class="pr-4">0 MB</span>
            <% end %>
              Created <%= format_creation_date(session.created_at) %>
            </div>
          </div>
          <.menu id={"session-#{session.id}-menu"}>
            <:toggle>
              <button class="icon-button" aria-label="open session menu" type="button">
                <.remix_icon icon="more-2-fill" class="text-xl" />
              </button>
            </:toggle>
            <:content>
              <button class="menu-item text-gray-500"
                type="button"
                role="menuitem"
                phx-click="fork_session"
                phx-value-id={session.id}>
                <.remix_icon icon="git-branch-line" />
                <span class="font-medium">Fork</span>
              </button>
              <a class="menu-item text-gray-500"
                role="menuitem"
                href={live_dashboard_process_path(@socket, session.pid)}
                target="_blank">
                <.remix_icon icon="dashboard-2-line" />
                <span class="font-medium">See on Dashboard</span>
              </a>
              <button class="menu-item text-gray-500"
                type="button"
                disabled={!session.memory_usage.runtime}
                role="menuitem"
                phx-click={toggle_edit(:off) |> JS.push("disconnect_runtime")}
                phx-value-id={session.id}>
                <.remix_icon icon="shut-down-line" />
                <span class="font-medium">Disconnect runtime</span>
              </button>
              <%= live_patch to: Routes.home_path(@socket, :close_session, session.id),
                    class: "menu-item text-red-600",
                    role: "menuitem" do %>
                <.remix_icon icon="close-circle-line" />
                <span class="font-medium">Close</span>
              <% end %>
            </:content>
          </.menu>
        </div>
      <% end %>
    </div>
    """
  end

  defp memory_info(%{memory: %{free: free, total: total}} = assigns) do
    used = total - free
    percentage = Float.round(used / total * 100, 2)
    assigns = assign(assigns, free: free, used: used, total: total, percentage: percentage)

    ~H"""
    <div class="pr-4">
      <span class="tooltip top" data-tooltip={"#{format_bytes(@free)} available"}>
      <svg viewbox="-10 5 50 25" width="30" height="30" xmlns="http://www.w3.org/2000/svg">
        <circle cx="16.91549431" cy="16.91549431" r="15.91549431"
          stroke="#E0E8F0" stroke-width="13" fill="none" />
        <circle cx="16.91549431" cy="16.91549431" r="15.91549431"
          stroke="#3E64FF" stroke-dasharray={"#{@percentage},100"} stroke-width="13" fill="none" />
      </svg>
      <div class="hidden md:flex">
        <span class="px-2 py-1 text-sm text-gray-500 font-medium">
          <%= format_bytes(@used) %> / <%= format_bytes(@total) %>
        </span>
      </div>
      </span>
    </div>
    """
  end

  defp edit_sessions(assigns) do
    ~H"""
    <div class="mx-4 mr-2 text-gray-600 flex flex-row gap-1">
      <.menu id="edit-sessions">
        <:toggle>
          <button id="toggle-edit" class="w-28 button-base button-outlined-gray px-4 pl-2 py-1"
            phx-click={toggle_edit(:on)} type="button">
            <.remix_icon icon="list-check-2" class="text-lg leading-none align-middle ml-1" />
            <span>Edit</span>
          </button>
          <button class="hidden w-28 button-base button-outlined-gray px-4 py-1 flex justify-between items-center"
            data-element="bulk-edit-member"
            type="button">
            <span>Actions</span>
            <.remix_icon icon="arrow-down-s-line" class="text-lg leading-none align-middle ml-1" />
          </button>
        </:toggle>
        <:content>
          <button class="menu-item text-gray-600" phx-click={toggle_edit(:off)} type="button">
            <.remix_icon icon="close-line" />
            <span class="font-medium">Cancel</span>
          </button>
          <button class="menu-item text-gray-600" phx-click={select_all()} type="button">
            <.remix_icon icon="checkbox-multiple-line" />
            <span class="font-medium">Select all</span>
          </button>
          <button class="menu-item text-gray-600" name="disconnect" type="button"
            phx-click={set_action("disconnect")}>
            <.remix_icon icon="shut-down-line" />
            <span class="font-medium">Disconnect runtime</span>
          </button>
          <button class="menu-item text-red-600" name="close_all" type="button"
            phx-click={set_action("close_all")}>
            <.remix_icon icon="close-circle-line" />
            <span class="font-medium">Close sessions</span>
          </button>
          <input id="bulk-action-input" class="hidden" type="text" name="action"/>
        </:content>
      </.menu>
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

  def toggle_edit(:on) do
    JS.remove_class("hidden", to: "[data-element='bulk-edit-member']")
    |> JS.add_class("hidden", to: "#toggle-edit")
    |> JS.dispatch("lb:session_list:on_selection_change")
  end

  def toggle_edit(:off) do
    JS.add_class("hidden", to: "[data-element='bulk-edit-member']")
    |> JS.remove_class("hidden", to: "#toggle-edit")
    |> JS.dispatch("lb:uncheck", to: "[name='session_ids[]']")
    |> JS.dispatch("lb:session_list:on_selection_change")
  end

  defp order_by_label("date"), do: "Date"
  defp order_by_label("title"), do: "Title"
  defp order_by_label("memory"), do: "Memory"

  defp order_by_icon("date"), do: "calendar-2-line"
  defp order_by_icon("title"), do: "text"
  defp order_by_icon("memory"), do: "cpu-line"

  defp sort_sessions(sessions, "date") do
    Enum.sort_by(sessions, & &1.created_at, {:desc, DateTime})
  end

  defp sort_sessions(sessions, "title") do
    Enum.sort_by(sessions, fn session ->
      {session.notebook_name, -DateTime.to_unix(session.created_at)}
    end)
  end

  defp sort_sessions(sessions, "memory") do
    Enum.sort_by(sessions, &total_runtime_memory/1, :desc)
  end

  defp total_runtime_memory(%{memory_usage: %{runtime: nil}}), do: 0
  defp total_runtime_memory(%{memory_usage: %{runtime: %{total: total}}}), do: total

  defp select_all() do
    JS.dispatch("lb:check", to: "[name='session_ids[]']")
    |> JS.dispatch("lb:session_list:on_selection_change")
  end

  defp set_action(action) do
    JS.dispatch("lb:set_value", to: "#bulk-action-input", detail: %{value: action})
    |> JS.dispatch("submit", to: "#bulk-action-form")
  end
end
