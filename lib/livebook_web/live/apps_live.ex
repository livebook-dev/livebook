defmodule LivebookWeb.AppsLive do
  use LivebookWeb, :live_view

  import LivebookWeb.AppHelpers

  alias LivebookWeb.LayoutHelpers

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Apps.subscribe()
    end

    apps = Livebook.Apps.list_apps()

    {:ok, assign(socket, apps: apps, page_title: "Apps - Livebook")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout
      current_page={~p"/apps"}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <div class="p-4 md:px-12 md:py-7 max-w-screen-lg mx-auto">
        <LayoutHelpers.title text="Apps" />
        <div class="mt-10">
          <.app_list apps={@apps} />
        </div>
      </div>
    </LayoutHelpers.layout>
    """
  end

  defp app_list(%{apps: []} = assigns) do
    ~H"""
    <.no_entries>
      You do not have any apps running. <br />
      You can deploy new apps by opening a notebook and clicking
      <.remix_icon icon="rocket-line" class="align-sub text-lg" /> in the sidebar.
    </.no_entries>
    """
  end

  defp app_list(assigns) do
    ~H"""
    <div class="flex flex-col space-y-16">
      <div :for={app <- Enum.sort_by(@apps, & &1.slug)} data-app-slug={app.slug}>
        <div class="mb-2 text-gray-800 font-medium text-xl">
          <%= "/" <> app.slug %>
        </div>
        <div class="mt-4 flex flex-col gap-3">
          <.message_box :for={warning <- app.warnings} kind={:warning} message={warning} />
        </div>
        <div class="mt-4 mb-2 text-gray-600 font-medium text-sm">
          App info
        </div>
        <div class="border border-gray-200 rounded-lg flex justify-between p-4">
          <div class="flex flex-col md:flex-row flex-wrap gap-4 md:gap-8 w-full max-w-2xl">
            <div class="flex-1">
              <.labeled_text label="Name" one_line>
                <%= app.notebook_name %>
              </.labeled_text>
            </div>
            <div class="flex-1">
              <.labeled_text label="URL" one_line>
                <a href={~p"/apps/#{app.slug}"}>
                  <%= ~p"/apps/#{app.slug}" %>
                </a>
              </.labeled_text>
            </div>
            <div class="flex-1">
              <.labeled_text label="Version" one_line>
                v<%= app.version %>
              </.labeled_text>
            </div>
            <div class="flex-1">
              <.labeled_text label="Session type" one_line>
                <%= if(app.multi_session, do: "Multi", else: "Single") %>
              </.labeled_text>
            </div>
          </div>
          <div class="flex flex-col md:flex-row md:items-center gap-2">
            <span class="tooltip top" data-tooltip="Terminate">
              <button
                class="icon-button"
                aria-label="terminate app"
                phx-click={JS.push("terminate_app", value: %{slug: app.slug})}
              >
                <.remix_icon icon="delete-bin-6-line" class="text-lg" />
              </button>
            </span>
          </div>
        </div>
        <div>
          <div class="mt-5 mb-2 text-gray-600 font-medium text-sm">
            Running sessions
          </div>
          <.table rows={app.sessions}>
            <:col :let={app_session} label="Status">
              <a
                aria-label="debug app"
                href={app_session.app_status == :error && ~p"/sessions/#{app_session.id}"}
                target="_blank"
              >
                <.app_status status={app_session.app_status} />
              </a>
            </:col>
            <:col :let={app_session} label="Uptime">
              <%= format_datetime_relatively(app_session.created_at) %>
            </:col>
            <:col :let={app_session} label="Version" align={:center}>
              v<%= app_session.version %>
            </:col>
            <:col :let={app_session} label="Clients" align={:center}>
              <%= app_session.client_count %>
            </:col>
            <:actions :let={app_session}>
              <span class="tooltip left" data-tooltip="Open">
                <a
                  class={["icon-button", app_session.app_status.lifecycle != :active && "disabled"]}
                  aria-label="open app"
                  href={~p"/apps/#{app.slug}/#{app_session.id}"}
                >
                  <.remix_icon icon="link" class="text-lg" />
                </a>
              </span>
              <span class="tooltip left" data-tooltip="Debug">
                <a class="icon-button" aria-label="debug app" href={~p"/sessions/#{app_session.id}"}>
                  <.remix_icon icon="terminal-line" class="text-lg" />
                </a>
              </span>
              <%= if app_session.app_status.lifecycle == :active do %>
                <span class="tooltip left" data-tooltip="Deactivate">
                  <button
                    class="icon-button"
                    aria-label="deactivate app session"
                    phx-click={
                      JS.push("deactivate_app_session",
                        value: %{slug: app.slug, session_id: app_session.id}
                      )
                    }
                  >
                    <.remix_icon icon="stop-circle-line" class="text-lg" />
                  </button>
                </span>
              <% else %>
                <span class="tooltip left" data-tooltip="Terminate">
                  <button
                    class="icon-button"
                    aria-label="terminate app session"
                    phx-click={
                      JS.push("terminate_app_session",
                        value: %{slug: app.slug, session_id: app_session.id}
                      )
                    }
                  >
                    <.remix_icon icon="delete-bin-6-line" class="text-lg" />
                  </button>
                </span>
              <% end %>
            </:actions>
          </.table>
        </div>
      </div>
    </div>
    """
  end

  defp table(assigns) do
    ~H"""
    <div class="overflow-x-auto border border-gray-200 rounded-lg">
      <table class="w-full border-collapse">
        <thead>
          <tr>
            <th
              :for={col <- @col}
              class={["px-4 py-2 text-gray-500 text-sm font-normal", align_to_class(col[:align])]}
            >
              <%= col[:label] %>
            </th>
            <th />
          </tr>
        </thead>
        <tbody>
          <tr
            :for={row <- @rows}
            class="whitespace-nowrap border-y last:border-b-0 border-gray-200 border-dashed hover:bg-gray-50"
          >
            <td
              :for={col <- @col}
              class={["px-4 py-2 text-gray-800 text-sm font-semibold", align_to_class(col[:align])]}
            >
              <%= render_slot(col, row) %>
            </td>
            <td class="px-4 py-2">
              <div class="flex flex-row items-center justify-end gap-2">
                <%= render_slot(@actions, row) %>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
    """
  end

  defp align_to_class(:right), do: "text-right"
  defp align_to_class(:center), do: "text-center"
  defp align_to_class(_), do: "text-left"

  @impl true
  def handle_info({type, _app} = event, socket)
      when type in [:app_created, :app_updated, :app_closed] do
    {:noreply, update(socket, :apps, &update_app_list(&1, event))}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  @impl true
  def handle_event("terminate_app", %{"slug" => slug}, socket) do
    app = Enum.find(socket.assigns.apps, &(&1.slug == slug))
    {:noreply, confirm_app_termination(socket, app.pid)}
  end

  def handle_event("terminate_app_session", %{"slug" => slug, "session_id" => session_id}, socket) do
    app_session = find_app_session(socket.assigns.apps, slug, session_id)
    Livebook.Session.close(app_session.pid)
    {:noreply, socket}
  end

  def handle_event(
        "deactivate_app_session",
        %{"slug" => slug, "session_id" => session_id},
        socket
      ) do
    app_session = find_app_session(socket.assigns.apps, slug, session_id)
    Livebook.Session.app_deactivate(app_session.pid)
    {:noreply, socket}
  end

  defp find_app_session(apps, slug, session_id) do
    app = Enum.find(apps, &(&1.slug == slug))
    Enum.find(app.sessions, &(&1.id == session_id))
  end

  def update_app_list(apps, {:app_created, app}) do
    if app in apps, do: apps, else: [app | apps]
  end

  def update_app_list(apps, {:app_updated, app}) do
    Enum.map(apps, fn other ->
      if other.slug == app.slug, do: app, else: other
    end)
  end

  def update_app_list(apps, {:app_closed, app}) do
    Enum.reject(apps, &(&1.slug == app.slug))
  end
end
