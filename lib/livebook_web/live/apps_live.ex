defmodule LivebookWeb.AppsLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Apps.subscribe()
    end

    apps = Livebook.Apps.list_apps()
    empty_apps_path? = Livebook.Apps.empty_apps_path?()

    {:ok, assign(socket, apps: apps, empty_apps_path?: empty_apps_path?)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-full flex flex-col overflow-y-auto">
      <div class="px-4 py-3 flex items-center justify-between">
        <div class="w-10 h-10">
          <.link navigate={~p"/"}>
            <img src={~p"/images/logo.png"} height="40" widthz="40" alt="logo livebook" />
          </.link>
        </div>
        <div>
          <.link navigate={~p"/apps-dashboard"} class="flex items-center text-blue-600">
            <span class="font-semibold">Dashboard</span>
            <.remix_icon icon="arrow-right-line" class="align-middle ml-1" />
          </.link>
        </div>
      </div>
      <div class="w-full max-w-screen-lg px-4 md:px-20 py-4 mx-auto">
        <div class="flex flex-col items-center">
          <h1 class="text-2xl text-gray-800 font-medium">
            Apps
          </h1>
          <div :if={@apps != []} class="w-full mt-5 max-w-[400px]">
            <div class="w-full flex flex-col space-y-4">
              <.link
                :for={app <- apps_listing(@apps)}
                navigate={~p"/apps/#{app.slug}"}
                class="px-4 py-3 border border-gray-200 rounded-xl text-gray-800 pointer hover:bg-gray-50 flex items-center justify-between"
              >
                <span class="font-semibold">{app.notebook_name}</span>
                <.remix_icon :if={not app.public?} icon="lock-password-line" />
              </.link>
            </div>
          </div>
          <div
            :if={@apps == [] and not @empty_apps_path?}
            class="mt-5 flex flex-col w-full max-w-[400px]"
          >
            <.no_entries :if={@apps == []}>
              No apps running.
            </.no_entries>
          </div>
          <div :if={@apps == [] and @empty_apps_path?} class="mt-5 text-gray-600">
            <div>
              No app notebooks found. Follow these steps to list your apps here:
            </div>
            <ol class="mt-4 pl-4 flex flex-col space-y-1 list-decimal list-inside">
              <li>
                Open a notebook
              </li>
              <li>
                Click <.remix_icon icon="rocket-line" class="align-baseline text-lg" />
                in the sidebar and configure the app as public
              </li>
              <li>
                Save the notebook to the
                <span class="font-medium">{Livebook.Config.apps_path()}</span>
                folder
              </li>
              <li>
                Relaunch your Livebook app
              </li>
            </ol>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_info({type, _app} = event, socket)
      when type in [:app_created, :app_updated, :app_closed] do
    {:noreply, update(socket, :apps, &LivebookWeb.AppComponents.update_app_list(&1, event))}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp apps_listing(apps) do
    Enum.sort_by(apps, & &1.notebook_name)
  end
end
