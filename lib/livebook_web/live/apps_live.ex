defmodule LivebookWeb.AppsLive do
  use LivebookWeb, :live_view

  @events [
    :app_folder_created,
    :app_folder_updated,
    :app_folder_deleted
  ]

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Teams.Broadcasts.subscribe([:app_server, :app_folders])
      Livebook.Apps.subscribe()
    end

    {:ok,
     socket
     |> assign(search_term: "", selected_app_folder: "")
     |> load_data()}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-full flex flex-col overflow-y-auto bg-white">
      <div class="px-6 py-4 bg-white border-b border-gray-200 flex items-center justify-between">
        <div class="w-10 h-10">
          <.menu id="apps-menu" position="bottom-right" md_position="bottom-left">
            <:toggle>
              <button class="flex items-center text-gray-900">
                <img src={~p"/images/logo.png"} height="40" width="40" alt="logo livebook" />
                <.remix_icon :if={@logout_enabled?} icon="arrow-down-s-line" />
              </button>
            </:toggle>
            <.menu_item :if={@logout_enabled?}>
              <button phx-click="logout" role="menuitem">
                <.remix_icon icon="logout-box-line" />
                <span>Logout</span>
              </button>
            </.menu_item>
          </.menu>
        </div>
        <div>
          <.link
            navigate={~p"/apps-dashboard"}
            class="flex items-center text-blue-600 hover:text-blue-700 transition-colors"
          >
            <span class="font-semibold">Dashboard</span>
            <.remix_icon icon="arrow-right-line" class="align-middle ml-1" />
          </.link>
        </div>
      </div>

      <div class="flex-1 px-6 py-6">
        <div class="max-w-7xl mx-auto">
          <h1 class="text-3xl font-bold text-gray-900 mb-2">Apps</h1>
          <p class="text-gray-600">Find your Livebook applications</p>

          <%= if @apps != [] do %>
            <div class="mb-10">
              <div class="flex flex-col md:flex-row gap-4">
                <div class="flex-1">
                  <div class="relative">
                    <.remix_icon
                      icon="search-line"
                      class="absolute left-3 bottom-[8px] text-gray-400"
                    />
                    <.text_field
                      id="search-app"
                      name="search_term"
                      placeholder="Search apps..."
                      value={@search_term}
                      phx-keyup="search"
                      phx-debounce="300"
                      class="w-full mt-6 pl-10 pr-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent transition-all"
                    />
                  </div>
                </div>
                <div class="md:w-48">
                  <form id="select-app-folder-form" phx-change="select_app_folder" phx-nosubmit>
                    <.select_field
                      id="select-app-folder"
                      name="app_folder"
                      label="Folder"
                      prompt="Select a folder..."
                      value={@selected_app_folder}
                      options={@app_folder_options}
                    />
                  </form>
                </div>
              </div>
            </div>

            <div
              :if={@filtered_apps == []}
              class="bg-white rounded-lg shadow-sm border border-gray-200 p-12 text-center"
            >
              <.remix_icon icon="windy-line" class="text-gray-400 text-2xl" />
              <h3 class="text-lg font-medium text-gray-900">No apps found</h3>
              <p class="text-gray-600">Try adjusting your search or filter criteria</p>
            </div>
            <div
              :for={{app_folder, id, icon, apps} <- @grouped_apps}
              :if={@filtered_apps != []}
              id={id}
              class="mb-8"
            >
              <h2 class="text-xl font-semibold text-gray-900 mb-4 flex items-center">
                <.remix_icon icon={icon} class="mr-2" />
                {app_folder}
                <span class="ml-2 text-sm font-normal text-gray-500">({length(apps)})</span>
              </h2>
              <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
                <.link
                  :for={app <- apps_listing(apps)}
                  id={"app-#{app.slug}"}
                  navigate={~p"/apps/#{app.slug}"}
                  class="border bg-gray-50 border-gray-300 rounded-lg p-4 hover:shadow-md hover:border-blue-300 transition-all duration-200"
                >
                  <div class="flex items-center justify-between">
                    <h3 class="text-sm font-medium text-gray-900 group-hover:text-blue-600 transition-colors truncate">
                      {app.notebook_name}
                    </h3>
                    <div class="space-x-1 ml-2">
                      <.remix_icon
                        :if={not app.public?}
                        icon="lock-password-line"
                        class="h-4 w-4 text-gray-400"
                      />
                      <.remix_icon
                        icon="arrow-right-line"
                        class="h-4 w-4 text-gray-400 group-hover:text-blue-600 transition-colors"
                      />
                    </div>
                  </div>
                </.link>
              </div>
            </div>
          <% else %>
            <div
              :if={@empty_apps_path?}
              class="bg-white rounded-lg shadow-sm border border-gray-200 p-12 text-center"
            >
              <.remix_icon icon="folder-add-line" class="mx-auto h-16 w-16 text-gray-300 mb-6" />
              <h3 class="text-xl font-semibold text-gray-900 mb-4">No app notebooks found</h3>
              <p class="text-gray-600 mb-6 max-w-md mx-auto">
                Follow these steps to list your apps here:
              </p>
              <div class="bg-gray-50 rounded-lg p-6 text-left max-w-md mx-auto">
                <ol class="space-y-3 text-sm text-gray-700">
                  <li class="flex items-start">
                    <span class="inline-flex items-center justify-center w-6 h-6 bg-blue-100 text-blue-600 rounded-full text-xs font-medium mr-3 mt-0.5">
                      1
                    </span>
                    Open a notebook
                  </li>
                  <li class="flex items-start">
                    <span class="inline-flex items-center justify-center w-6 h-6 bg-blue-100 text-blue-600 rounded-full text-xs font-medium mr-3 mt-0.5">
                      2
                    </span>
                    <div>
                      Click <.remix_icon icon="rocket-line" class="inline align-baseline text-base" />
                      in the sidebar and configure the app as public
                    </div>
                  </li>
                  <li class="flex items-start">
                    <span class="inline-flex items-center justify-center w-6 h-6 bg-blue-100 text-blue-600 rounded-full text-xs font-medium mr-3 mt-0.5">
                      3
                    </span>
                    <div>
                      Save the notebook to the
                      <span class="font-medium bg-gray-100 px-1 rounded text-xs">
                        {Livebook.Config.apps_path()}
                      </span>
                      folder
                    </div>
                  </li>
                  <li class="flex items-start">
                    <span class="inline-flex items-center justify-center w-6 h-6 bg-blue-100 text-blue-600 rounded-full text-xs font-medium mr-3 mt-0.5">
                      4
                    </span>
                    Relaunch your Livebook app
                  </li>
                </ol>
              </div>
            </div>
            <div
              :if={not @empty_apps_path?}
              class="bg-white rounded-lg shadow-sm border border-gray-200 p-12 text-center"
            >
              <.remix_icon icon="file-line" class="mx-auto h-16 w-16 text-gray-300 mb-6" />
              <h3 class="text-xl font-semibold text-gray-900 mb-2">No apps running</h3>
              <p class="text-gray-600">Start some apps to see them listed here</p>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("search", %{"value" => search_term}, socket) do
    {:noreply,
     socket
     |> assign(search_term: search_term)
     |> load_data()}
  end

  def handle_event("select_app_folder", %{"app_folder" => app_folder_id}, socket) do
    {:noreply,
     socket
     |> assign(selected_app_folder: app_folder_id)
     |> load_data()}
  end

  @impl true
  def handle_info({type, _app} = event, socket)
      when type in [:app_created, :app_updated, :app_closed] do
    apps = LivebookWeb.AppComponents.update_app_list(socket.assigns.apps, event)
    {:noreply, load_data(socket, apps)}
  end

  def handle_info({:server_authorization_updated, _}, socket) do
    {:noreply, load_data(socket)}
  end

  def handle_info({type, _app_folder}, socket) when type in @events do
    {:noreply, load_data(socket)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp apps_listing(apps) do
    Enum.sort_by(apps, & &1.notebook_name)
  end

  defp load_data(socket, apps \\ nil) do
    apps = apps || Livebook.Apps.list_authorized_apps(socket.assigns.current_user)

    filtered_apps =
      filter_apps(apps, socket.assigns.search_term, socket.assigns.selected_app_folder)

    empty_apps_path? = Livebook.Apps.empty_apps_path?()

    app_folders =
      Enum.flat_map(Livebook.Hubs.get_hubs(), fn
        %{id: "team-" <> _} = team -> Livebook.Teams.get_app_folders(team)
        _ -> []
      end)

    app_folder_options =
      for app_folder <- app_folders do
        {app_folder.name, app_folder.id}
      end

    grouped_apps =
      filtered_apps
      |> Enum.group_by(& &1.app_spec.app_folder_id)
      |> Enum.map(fn
        {nil, apps} ->
          {"Ungrouped apps", "ungrouped-apps", "asterisk", apps}

        {id, apps} ->
          {Enum.find_value(app_folders, &(&1.id == id && &1.name)), "app-folder-#{id}",
           "folder-line", apps}
      end)
      |> Enum.sort_by(&elem(&1, 0))

    assign(socket,
      apps: apps,
      grouped_apps: grouped_apps,
      app_folders: app_folders,
      app_folder_options: app_folder_options,
      filtered_apps: filtered_apps,
      empty_apps_path?: empty_apps_path?,
      logout_enabled?:
        Livebook.Config.logout_enabled?() and socket.assigns.current_user.email != nil
    )
  end

  defp filter_apps(apps, term, app_folder_id) do
    apps
    |> search_apps(term)
    |> filter_by_app_folder(app_folder_id)
  end

  defp search_apps(apps, ""), do: apps

  defp search_apps(apps, term) do
    term = String.downcase(term)

    Enum.filter(apps, fn app ->
      String.contains?(String.downcase(app.notebook_name), term) or
        String.contains?(app.slug, term)
    end)
  end

  defp filter_by_app_folder(apps, ""), do: apps

  defp filter_by_app_folder(apps, app_folder_id) do
    Enum.filter(apps, &(&1.app_spec.app_folder_id == app_folder_id))
  end
end
