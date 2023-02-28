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
      <div class="p-4 md:px-12 md:py-7 max-w-screen-lg mx-auto space-y-4">
        <div>
          <LayoutHelpers.title text="Apps" />
          <p class="mt-4 mb-8 text-gray-700">
            <%= if @sessions == [] do %>
              No apps currently running.
            <% else %>
              These apps are currently running.
            <% end %>
          </p>
        </div>
        <div class="flex flex-col space-y-4">
          <div
            :for={session <- @sessions}
            class="border border-gray-200 rounded-lg flex flex-col"
            data-app-slug={session.app_info.slug}
          >
            <div class="flex justify-between px-4 py-3 text-gray-700 font-medium border-b border-gray-200 bg-gray-100">
              <div>
                <%= session.notebook_name %>
              </div>
              <div class="flex space-x-2">
                <span class="tooltip top" data-tooltip="Debug">
                  <a
                    class="icon-button"
                    aria-label="debug app"
                    href={~p"/sessions/#{session.id}"}
                    target="_blank"
                  >
                    <.remix_icon icon="terminal-line" class="text-lg" />
                  </a>
                </span>
                <span class="tooltip top" data-tooltip="Terminate">
                  <button
                    class="icon-button"
                    aria-label="terminate app"
                    phx-click={JS.push("terminate_app", value: %{session_id: session.id})}
                  >
                    <.remix_icon icon="delete-bin-6-line" class="text-lg" />
                  </button>
                </span>
              </div>
            </div>
            <div class="flex p-4 space-x-8">
              <.labeled_text label="Status">
                <.app_status status={session.app_info.status} />
              </.labeled_text>
              <.labeled_text label="URL" one_line>
                <%= if session.app_info.registered do %>
                  <a href={url(~p"/apps/#{session.app_info.slug}")} target="_blank">
                    <%= url(~p"/apps/#{session.app_info.slug}") %>
                  </a>
                <% else %>
                  -
                <% end %>
              </.labeled_text>
            </div>
          </div>
        </div>
      </div>
    </LayoutHelpers.layout>
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
end
