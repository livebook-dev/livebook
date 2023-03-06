defmodule LivebookWeb.AppsLive do
  use LivebookWeb, :live_view

  import LivebookWeb.AppHelpers
  import LivebookWeb.SessionHelpers

  alias LivebookWeb.LayoutHelpers
  alias Livebook.Sessions

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Sessions.subscribe()
    end

    sessions =
      Sessions.list_sessions()
      |> Enum.filter(&(&1.mode == :app))
      |> Enum.sort_by(& &1.created_at, {:desc, DateTime})

    {:ok, assign(socket, sessions: sessions, page_title: "Livebook - Apps")}
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
        <div class="mt-8">
          <.app_list sessions={@sessions} />
        </div>
      </div>
    </LayoutHelpers.layout>
    """
  end

  defp app_list(%{sessions: []} = assigns) do
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
    <div class="flex flex-col space-y-8">
      <div :for={{slug, sessions} <- group_apps(@sessions)}>
        <div class="mb-2 text-gray-800 font-medium text-lg">
          <%= "/" <> slug %>
        </div>
        <div class="flex flex-col">
          <%= for {session, idx} <- Enum.with_index(sessions) do %>
            <div :if={idx > 0} class="ml-4 border-l-2 border-gray-300 border-dashed h-6"></div>
            <.app_box session={session} />
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  defp app_box(assigns) do
    ~H"""
    <div
      class="border border-gray-200 rounded-lg flex justify-between p-4"
      data-app-slug={@session.app_info.slug}
    >
      <div class="flex flex-col md:flex-row space-y-4 md:space-y-0 md:space-x-8 w-full max-w-2xl">
        <div class="flex-1">
          <.labeled_text label="Status">
            <.app_status status={@session.app_info.status} />
          </.labeled_text>
        </div>
        <div class="flex-1">
          <.labeled_text label="Name">
            <%= @session.notebook_name %>
          </.labeled_text>
        </div>
        <div class="flex-1 grow-[2]">
          <.labeled_text label="URL">
            <%= if @session.app_info.registered do %>
              <a href={url(~p"/apps/#{@session.app_info.slug}")} target="_blank">
                <%= url(~p"/apps/#{@session.app_info.slug}") %>
              </a>
            <% else %>
              -
            <% end %>
          </.labeled_text>
        </div>
      </div>
      <div class="flex flex-col md:flex-row gap-2">
        <span class="tooltip top" data-tooltip="Debug">
          <a
            class="icon-button"
            aria-label="debug app"
            href={~p"/sessions/#{@session.id}"}
            target="_blank"
          >
            <.remix_icon icon="terminal-line" class="text-lg" />
          </a>
        </span>
        <%= if @session.app_info.registered do %>
          <span class="tooltip top" data-tooltip="Stop">
            <button
              class="icon-button"
              aria-label="stop app"
              phx-click={JS.push("stop_app", value: %{session_id: @session.id})}
            >
              <.remix_icon icon="stop-circle-line" class="text-lg" />
            </button>
          </span>
        <% else %>
          <span class="tooltip top" data-tooltip="Terminate">
            <button
              class="icon-button"
              aria-label="terminate app"
              phx-click={JS.push("terminate_app", value: %{session_id: @session.id})}
            >
              <.remix_icon icon="delete-bin-6-line" class="text-lg" />
            </button>
          </span>
        <% end %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_info({type, session} = event, socket)
      when type in [:session_created, :session_updated, :session_closed] and session.mode == :app do
    {:noreply, update(socket, :sessions, &update_session_list(&1, event))}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  @impl true
  def handle_event("terminate_app", %{"session_id" => session_id}, socket) do
    session = Enum.find(socket.assigns.sessions, &(&1.id == session_id))
    Livebook.Session.close(session.pid)
    {:noreply, socket}
  end

  def handle_event("stop_app", %{"session_id" => session_id}, socket) do
    session = Enum.find(socket.assigns.sessions, &(&1.id == session_id))
    Livebook.Session.app_stop(session.pid)
    {:noreply, socket}
  end

  defp group_apps(sessions) do
    sessions
    |> Enum.group_by(& &1.app_info.slug)
    |> Enum.sort_by(&elem(&1, 0))
  end
end
