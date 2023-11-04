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
    <div class="w-full h-full px-4 py-8 flex justify-center items-center">
      <div class="w-full flex flex-col items-center">
        <div class="text-gray-700 text-xl font-medium">
          Apps
        </div>
        <div class="w-full mt-5 mx-auto max-w-[400px]">
          <div class="w-full flex flex-col space-y-4">
            <.link
              :for={app <- apps_listing(@apps)}
              href={~p"/apps/#{app.slug}"}
              class="px-4 py-3 border border-gray-200 rounded-xl text-gray-800 pointer hover:bg-gray-50 flex items-center justify-between"
            >
              <span class="font-semibold"><%= app.notebook_name %></span>
              <.remix_icon :if={not app.public?} icon="lock-password-line" />
            </.link>
          </div>
        </div>
        <%!-- TODO show message when there are no apps --%>
        <div :if={@apps == [] and @empty_apps_path?} class="mt-5 text-gray-600">
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
    </div>
    """
  end

  @impl true
  def handle_info({type, _app} = event, socket)
      when type in [:app_created, :app_updated, :app_closed] do
    {:noreply, update(socket, :apps, &LivebookWeb.AppHelpers.update_app_list(&1, event))}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp apps_listing(apps) do
    Enum.sort_by(apps, & &1.notebook_name)
  end
end
