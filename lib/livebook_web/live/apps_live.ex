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
      <.remix_icon icon="rocket-line" class="align-top text-lg" /> in the sidebar.
    </.no_entries>
    """
  end

  defp app_list(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <div :for={app <- Enum.sort_by(@apps, & &1.slug)} data-app-slug={app.slug}>
        <a
          phx-click={JS.toggle(to: "[data-app-slug=#{app.slug}] .toggle")}
          class="flex items-center justify-between break-all mb-2 text-gray-800 font-medium text-xl hover:cursor-pointer"
        >
          <%= "/" <> app.slug %>
          <.remix_icon icon="arrow-drop-up-line" class="text-3xl text-gray-400 toggle" />
          <.remix_icon icon="arrow-drop-down-line" class="text-3xl text-gray-400 hidden toggle" />
        </a>
        <div class="toggle">
          <div class="mt-4 flex flex-col gap-3">
            <.message_box :for={warning <- app.warnings} kind={:warning} message={warning} />
          </div>
          <div class="flex-col mb-8">
            <div class="p-4 border-x border-t border-gray-200 rounded-t-lg ">
              <div class="uppercase text-gray-500 text-sm font-medium leading-normal tracking-wider">
                App Info
              </div>
              <div class="grid grid-cols-1 md:grid-cols-3 lg:grid-cols-[minmax(0,_2fr)_minmax(0,_2fr)_minmax(0,_1fr)_minmax(0,_1fr)_minmax(0,_1fr)] gap-4 mt-3">
                <div class="break-words">
                  <.labeled_text label="Name">
                    <%= app.notebook_name %>
                  </.labeled_text>
                </div>
                <div class="break-all">
                  <.labeled_text label="URL">
                    <a href={~p"/apps/#{app.slug}"}>
                      <%= ~p"/apps/#{app.slug}" %>
                    </a>
                  </.labeled_text>
                </div>
                <div>
                  <.labeled_text label="Latest version" one_line>
                    v<%= app.version %>
                  </.labeled_text>
                </div>
                <div>
                  <.labeled_text label="Session type" one_line>
                    <%= if(app.multi_session, do: "Multi", else: "Single") %>
                  </.labeled_text>
                </div>
                <div class="flex flex-col md:flex-row md:items-center justify-start lg:justify-end">
                  <span class="tooltip top" data-tooltip="Terminate">
                    <button
                      class="icon-button text-right"
                      aria-label="terminate app"
                      phx-click={JS.push("terminate_app", value: %{slug: app.slug})}
                    >
                      <.remix_icon icon="delete-bin-6-line" class="text-lg" />
                    </button>
                  </span>
                </div>
              </div>
            </div>
            <div class="border border-gray-200 rounded-b-lg overflow-auto tiny-scrollbar whitespace-none">
              <%= if Enum.any?(app.sessions) do %>
                <div class="uppercase text-gray-500 text-sm font-medium leading-normal tracking-wider px-4 pt-4 pb-3">
                  Running sessions
                </div>
                <.grid rows={app.sessions}>
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
                  <:col :let={app_session} label="Version">
                    v<%= app_session.version %>
                  </:col>
                  <:col :let={app_session} label="Clients">
                    <%= app_session.client_count %>
                  </:col>
                  <:actions :let={app_session}>
                    <span class="tooltip left" data-tooltip="Open">
                      <a
                        class={[
                          "icon-button",
                          app_session.app_status.lifecycle != :active && "disabled"
                        ]}
                        aria-label="open app"
                        href={~p"/apps/#{app.slug}/#{app_session.id}"}
                      >
                        <.remix_icon icon="link" class="text-lg" />
                      </a>
                    </span>
                    <span class="tooltip left" data-tooltip="Debug">
                      <a
                        class="icon-button"
                        aria-label="debug app"
                        href={~p"/sessions/#{app_session.id}"}
                      >
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
                </.grid>
              <% else %>
                <div class="p-4 uppercase text-gray-500 text-sm font-medium leading-normal tracking-wider">
                  No running sessions
                </div>
              <% end %>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp grid(assigns) do
    ~H"""
    <div class="min-w-[650px]">
      <div class="px-2 pb-2">
        <div class="grid grid-cols-[minmax(0,_0.5fr)_minmax(0,_0.75fr)_minmax(0,_0.5fr)_minmax(0,_0.5fr)_minmax(0,_0.5fr)] md:grid-cols-[minmax(0,_2fr)_minmax(0,_2fr)_minmax(0,_1fr)_minmax(0,_1fr)_minmax(0,_1fr)] gap-4 px-2">
          <div
            :for={col <- @col}
            class={["text-gray-500 text-sm font-normal", align_to_class(col[:align])]}
          >
            <%= col[:label] %>
          </div>
        </div>
        <div :for={row <- @rows} class="whitespace-nowrap px-2 hover:bg-gray-50 hover:rounded-md">
          <div class="grid grid-cols-[minmax(0,_0.5fr)_minmax(0,_0.75fr)_minmax(0,_0.5fr)_minmax(0,_0.5fr)_minmax(0,_0.5fr)] md:grid-cols-[minmax(0,_2fr)_minmax(0,_2fr)_minmax(0,_1fr)_minmax(0,_1fr)_minmax(0,_1fr)] gap-4">
            <div
              :for={col <- @col}
              class={["py-2 text-gray-800 text-sm font-semibold", align_to_class(col[:align])]}
            >
              <%= render_slot(col, row) %>
            </div>
            <div class="py-2 flex flex-row items-center justify-end gap-2">
              <%= render_slot(@actions, row) %>
            </div>
          </div>
        </div>
      </div>
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
