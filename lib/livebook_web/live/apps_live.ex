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

    empty_apps_path? = Livebook.Apps.empty_apps_path?()

    {:ok,
     socket
     |> assign(
       search_term: "",
       selected_app_folder: "",
       apps: Livebook.Apps.list_authorized_apps(socket.assigns.current_user),
       empty_apps_path?: empty_apps_path?,
       logout_enabled?:
         Livebook.Config.logout_enabled?() and socket.assigns.current_user.email != nil
     )
     |> load_app_folders()
     |> apply_filters()}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-full flex flex-col overflow-y-auto bg-white">
      <div class="border-b border-gray-200/70 bg-white/80 backdrop-blur-sm shadow-sm">
        <div class="max-w-6xl mx-auto px-10 py-3.5 flex items-center justify-between">
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
              class="inline-flex items-center gap-1.5 text-gray-500 text-xs font-medium px-3 py-1.5 rounded-md hover:bg-gray-100/80 hover:text-gray-700 transition-all duration-200"
            >
              <span>Dashboard</span>
              <.remix_icon
                icon="arrow-right-line"
                class="text-xs text-gray-400"
              />
            </.link>
          </div>
        </div>
      </div>
      <div class="flex-1 overflow-y-auto">
        <div class="max-w-6xl mx-auto px-12 py-12 pb-24">
          <div class="mb-6">
            <h1 class="text-3xl font-semibold text-gray-900 tracking-tight leading-none m-0">
              Apps
            </h1>
          </div>
          <%= if @apps != [] do %>
            <div class="flex flex-col md:flex-row gap-4 mb-10">
              <div class="flex-1">
                <div class="relative">
                  <.remix_icon
                    icon="search-line"
                    class="absolute left-3 bottom-2 text-gray-400"
                  />
                  <.text_field
                    id="search-app"
                    name="search_term"
                    placeholder="Search by app name..."
                    value={@search_term}
                    phx-keyup="search"
                    phx-debounce="300"
                    class="w-full pl-10 text-base"
                    autofocus
                  />
                </div>
              </div>
              <div :if={@show_app_folders?} class="md:w-48">
                <form id="select-app-folder-form" phx-change="select_app_folder" phx-nosubmit>
                  <.select_field
                    id="select-app-folder"
                    name="app_folder"
                    prompt="All folders"
                    value={@selected_app_folder}
                    options={@app_folder_options}
                  />
                </form>
              </div>
            </div>

            <div :if={@filtered_apps == []} class="text-center py-20 px-5">
              <div class="text-5xl text-gray-300 mb-4">
                <.remix_icon icon="windy-line" />
              </div>
              <div class="text-base font-semibold text-gray-900 mb-2">No apps found</div>
              <div class="text-sm text-gray-500">Try adjusting your search or filter</div>
            </div>

            <div :if={@filtered_apps != []} class="flex flex-col gap-12">
              <div :for={{app_folder, id, icon, apps} <- @grouped_apps} id={id}>
                <%= if @show_app_folders? do %>
                  <div class="flex items-center gap-2.5 mb-5 pb-2.5 border-b border-gray-200/70">
                    <.remix_icon icon={icon} class="text-gray-500/80 text-xs leading-none" />
                    <span class="text-sm text-gray-500 flex-1 tracking-widest leading-none">
                      {app_folder}
                    </span>
                  </div>
                <% end %>
                <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3.5">
                  <.link
                    :for={app <- apps_listing(apps)}
                    id={"app-#{app.slug}"}
                    navigate={~p"/apps/#{app.slug}"}
                    class="group block border border-gray-200 rounded-lg px-4 py-4 bg-white shadow-sm hover:shadow-lg hover:border-gray-300 hover:-translate-y-1 transition-all duration-200 ease-out cursor-pointer"
                  >
                    <div class="flex items-center justify-between gap-3">
                      <span class="text-base font-semibold text-gray-800 flex-1 truncate group-hover:text-gray-900">
                        {app.notebook_name}
                      </span>
                      <div class="flex items-center gap-1.5 shrink-0">
                        <.remix_icon
                          :if={not app.public?}
                          icon="lock-password-line"
                          class="text-sm text-gray-300"
                        />
                      </div>
                    </div>
                  </.link>
                </div>
              </div>
            </div>
          <% else %>
            <div class="max-w-2xl mx-auto mt-16 text-center">
              <div :if={@empty_apps_path?}>
                <div class="mb-8">
                  <.remix_icon icon="windy-line" class="text-gray-300 text-5xl mb-4" />
                  <h3 class="text-lg font-semibold text-gray-900 mb-2">No apps found</h3>
                  <p class="text-sm text-gray-500 mb-6">
                    Follow these steps to list your apps here:
                  </p>
                </div>
                <div class="max-w-md mx-auto">
                  <ol class="space-y-3">
                    <li class="flex items-start gap-3 text-left text-sm text-gray-700 leading-relaxed">
                      <span class="inline-flex items-center justify-center w-6 h-6 bg-gray-100 text-gray-500 rounded-full text-xs font-semibold shrink-0">
                        1
                      </span>
                      <span>Open a notebook</span>
                    </li>
                    <li class="flex items-start gap-3 text-left text-sm text-gray-700 leading-relaxed">
                      <span class="inline-flex items-center justify-center w-6 h-6 bg-gray-100 text-gray-500 rounded-full text-xs font-semibold shrink-0 mt-px">
                        2
                      </span>
                      <div class="flex gap-x-1 items-center flex-wrap">
                        Click <.remix_icon icon="rocket-line" class="inline align-middle text-base" />
                        in the sidebar and configure the app as public
                      </div>
                    </li>
                    <li class="flex items-start gap-3 text-left text-sm text-gray-700 leading-relaxed">
                      <span class="inline-flex items-center justify-center w-6 h-6 bg-gray-100 text-gray-500 rounded-full text-xs font-semibold shrink-0 mt-px">
                        3
                      </span>
                      <div class="flex gap-x-1 items-center flex-wrap">
                        Save the notebook to the
                        <code class="inline-block px-1.5 py-0.5 bg-gray-100 text-gray-900 rounded text-xs font-mono leading-none">
                          {Livebook.Config.apps_path()}
                        </code>
                        folder
                      </div>
                    </li>
                    <li class="flex items-start gap-3 text-left text-sm text-gray-700 leading-relaxed">
                      <span class="inline-flex items-center justify-center w-6 h-6 bg-gray-100 text-gray-500 rounded-full text-xs font-semibold shrink-0">
                        4
                      </span>
                      <span>Relaunch your Livebook app</span>
                    </li>
                  </ol>
                </div>
              </div>
              <div :if={not @empty_apps_path?} class="text-center">
                <.remix_icon icon="windy-line" class="text-gray-300 text-3xl mb-4" />
                <h3 class="text-lg font-semibold text-gray-900">No apps running</h3>
              </div>
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
     |> apply_filters()}
  end

  def handle_event("select_app_folder", %{"app_folder" => app_folder_id}, socket) do
    {:noreply,
     socket
     |> assign(selected_app_folder: app_folder_id)
     |> apply_filters()}
  end

  @impl true
  def handle_info({type, _app}, socket) when type in [:app_created, :app_updated, :app_closed] do
    {:noreply,
     socket
     |> assign(apps: Livebook.Apps.list_authorized_apps(socket.assigns.current_user))
     |> apply_filters()}
  end

  def handle_info({:server_authorization_updated, _}, socket) do
    {:noreply,
     socket
     |> assign(
       apps: Livebook.Apps.list_authorized_apps(socket.assigns.current_user),
       logout_enabled?:
         Livebook.Config.logout_enabled?() and socket.assigns.current_user.email != nil
     )
     |> apply_filters()}
  end

  def handle_info({type, _app_folder}, socket) when type in @events do
    {:noreply,
     socket
     |> load_app_folders()
     |> apply_filters()}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp apps_listing(apps) do
    Enum.sort_by(apps, & &1.notebook_name)
  end

  def load_app_folders(socket) do
    app_folders =
      Enum.flat_map(Livebook.Hubs.get_hubs(), &Livebook.Hubs.Provider.get_app_folders/1)

    app_folder_options =
      for app_folder <- app_folders do
        {app_folder.name, app_folder.id}
      end

    assign(socket, app_folders: app_folders, app_folder_options: app_folder_options)
  end

  defp apply_filters(socket) do
    apps = socket.assigns.apps
    app_folders = socket.assigns.app_folders

    filtered_apps =
      filter_apps(apps, socket.assigns.search_term, socket.assigns.selected_app_folder)

    grouped_apps =
      filtered_apps
      |> Enum.group_by(fn
        %{app_spec: %{app_folder_id: id}} -> Enum.find_value(app_folders, &(&1.id == id && id))
        _ -> nil
      end)
      |> Enum.map(fn
        {nil, apps} ->
          {"No folder", "ungrouped-apps", "function-line", apps}

        {id, apps} ->
          app_folder_name = Enum.find_value(app_folders, &(&1.id == id && &1.name))
          {app_folder_name, "app-folder-#{id}", "folder-line", apps}
      end)
      |> Enum.sort_by(fn {name, _, _, _} ->
        if name == "No folder", do: {1, name}, else: {0, name}
      end)

    show_app_folders? = Enum.any?(apps, &is_struct(&1.app_spec, Livebook.Apps.TeamsAppSpec))

    assign(socket,
      grouped_apps: grouped_apps,
      filtered_apps: filtered_apps,
      show_app_folders?: show_app_folders?
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
    Enum.filter(apps, fn
      %{app_spec: %{app_folder_id: id}} -> id == app_folder_id
      _otherwise -> false
    end)
  end
end
