defmodule LivebookWeb.AuthAppListLive do
  use LivebookWeb, :live_view

  import LivebookWeb.SessionHelpers

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Sessions.subscribe()
    end

    sessions = Livebook.Sessions.list_sessions() |> Enum.filter(&(&1.mode == :app))

    {:ok,
     assign(socket,
       sessions: sessions,
       booting_count: booting_count(sessions),
       empty_apps_path?: Livebook.Apps.empty_apps_path?()
     ), layout: false}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="w-full flex flex-col items-center">
      <div class="text-gray-700 text-xl font-medium">
        Public apps
      </div>
      <div :if={@sessions != []} class="mt-5 max-w-[400px] w-full flex flex-col space-y-4">
        <.link
          :for={session <- visible_sessions(@sessions)}
          navigate={~p"/apps/#{session.app_info.slug}"}
          class="px-4 py-3 border border-gray-200 rounded-xl text-gray-800 pointer hover:bg-gray-50 flex justify-between"
        >
          <span class="font-semibold"><%= session.notebook_name %></span>
          <.remix_icon icon="arrow-right-line" class="" />
        </.link>
      </div>
      <div :if={@booting_count > 0} class="mt-2 text-gray-600 text-sm">
        <%= pluralize(@booting_count, "app", "apps") %> booting
      </div>
      <div :if={@empty_apps_path?} class="mt-5 text-gray-600">
        <div>
          No app notebooks found. <br />Follow these steps to list your apps here:
        </div>
        <ol class="mt-4 pl-4 flex flex-col space-y-1 list-decimal list-inside">
          <li>
            Open a notebook
          </li>
          <li>
            Click <.remix_icon icon="rocket-line" class="align-sub text-lg" />
            in the sidebar and configure the app as public
          </li>
          <li>
            Save the notebook to the
            <span class="font-medium"><%= Livebook.Config.apps_path() %></span>
            folder
          </li>
          <li>
            Relaunch your Livebook app
          </li>
        </ol>
      </div>
    </div>
    """
  end

  @impl true
  def handle_info({type, session} = event, socket)
      when type in [:session_created, :session_updated, :session_closed] and session.mode == :app do
    sessions = update_session_list(socket.assigns.sessions, event)
    {:noreply, assign(socket, sessions: sessions, booting_count: booting_count(sessions))}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp visible_sessions(sessions) do
    sessions
    |> Enum.filter(&(&1.app_info.public? and &1.app_info.registered))
    |> Enum.sort_by(& &1.notebook_name)
  end

  defp booting_count(sessions) do
    Enum.count(sessions, &(&1.app_info.public? and &1.app_info.status == :booting))
  end
end
