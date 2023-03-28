defmodule LivebookWeb.AuthAppListLive do
  use LivebookWeb, :live_view

  import LivebookWeb.AppHelpers
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
          class={[
            "px-4 py-3 border border-gray-200 rounded-xl text-gray-800 pointer hover:bg-gray-50 flex justify-between",
            not session.app_info.registered && "pointer-events-none"
          ]}
        >
          <span class="font-semibold"><%= session.notebook_name %></span>
          <%= if session.app_info.registered do %>
            <.remix_icon icon="arrow-right-line" class="" />
          <% else %>
            <div class="mr-0.5 flex">
              <.app_status status={session.app_info.status} show_label={false} />
            </div>
          <% end %>
        </.link>
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
    {:noreply, update(socket, :sessions, &update_session_list(&1, event))}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp visible_sessions(sessions) do
    sessions
    |> Enum.filter(& &1.app_info.public?)
    |> Enum.sort_by(& &1.notebook_name)
  end
end
