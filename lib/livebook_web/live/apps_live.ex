defmodule LivebookWeb.AppsLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Teams.Broadcasts.subscribe(:app_server)
      Livebook.Apps.subscribe()
    end

    apps = Livebook.Apps.list_authorized_apps(socket.assigns.current_user)
    empty_apps_path? = Livebook.Apps.empty_apps_path?()

    # Generate some mock data for demo purposes if there are few apps
    demo_apps = if length(apps) < 10, do: generate_demo_apps(apps), else: apps

    {:ok,
     assign(socket,
       apps: demo_apps,
       filtered_apps: demo_apps,
       search_term: "",
       selected_group: "all",
       empty_apps_path?: empty_apps_path?,
       logout_enabled?:
         Livebook.Config.logout_enabled?() and socket.assigns.current_user.email != nil
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-full flex flex-col overflow-y-auto bg-gray-50">
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
          <div class="mb-8">
            <h1 class="text-3xl font-bold text-gray-900 mb-2">Apps</h1>
            <p class="text-gray-600">Find and manage your Livebook applications</p>
          </div>

          <%= if @apps != [] do %>
            <div class="bg-white rounded-lg shadow-sm border border-gray-200 mb-6">
              <div class="p-6 border-b border-gray-200">
                <div class="flex flex-col md:flex-row gap-4">
                  <div class="flex-1">
                    <div class="relative">
                      <.remix_icon
                        icon="search-line"
                        class="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400"
                      />
                      <input
                        type="text"
                        placeholder="Search apps..."
                        value={@search_term}
                        phx-keyup="search"
                        phx-debounce="300"
                        class="w-full pl-10 pr-4 py-2.5 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent transition-all"
                      />
                    </div>
                  </div>
                  <div class="md:w-48">
                    <select
                      value={@selected_group}
                      phx-change="filter_group"
                      class="w-full px-3 py-2.5 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white"
                    >
                      <option value="all">All Apps</option>
                      <%= for group <- get_app_groups(@apps) do %>
                        <option value={group}>{group}</option>
                      <% end %>
                    </select>
                  </div>
                </div>
              </div>
            </div>

            <%= if @filtered_apps == [] do %>
              <div class="bg-white rounded-lg shadow-sm border border-gray-200 p-12 text-center">
                <.remix_icon icon="search-line" class="mx-auto h-12 w-12 text-gray-300 mb-4" />
                <h3 class="text-lg font-medium text-gray-900 mb-2">No apps found</h3>
                <p class="text-gray-600">Try adjusting your search or filter criteria</p>
              </div>
            <% else %>
              <%= for {group, apps} <- group_apps(@filtered_apps, @selected_group) do %>
                <div class="mb-8">
                  <h2 class="text-xl font-semibold text-gray-900 mb-4 flex items-center">
                    <.remix_icon icon="folder-line" class="mr-2" />
                    {group}
                    <span class="ml-2 text-sm font-normal text-gray-500">({length(apps)})</span>
                  </h2>
                  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
                    <%= for app <- Enum.sort_by(apps, & &1.notebook_name) do %>
                      <.link
                        navigate={~p"/apps/#{app.slug}"}
                        class="bg-white border border-gray-200 rounded-lg p-4 hover:shadow-md hover:border-blue-300 transition-all duration-200 group"
                      >
                        <div class="flex items-start justify-between mb-2">
                          <div class="flex-1 min-w-0">
                            <h3 class="text-sm font-medium text-gray-900 group-hover:text-blue-600 transition-colors truncate">
                              {app.notebook_name}
                            </h3>
                          </div>
                          <div class="flex items-center space-x-1 ml-2">
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
                    <% end %>
                  </div>
                </div>
              <% end %>
            <% end %>
          <% else %>
            <%= if @empty_apps_path? do %>
              <div class="bg-white rounded-lg shadow-sm border border-gray-200 p-12 text-center">
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
                        Click
                        <.remix_icon icon="rocket-line" class="inline align-baseline text-base" />
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
            <% else %>
              <div class="bg-white rounded-lg shadow-sm border border-gray-200 p-12 text-center">
                <.remix_icon icon="file-line" class="mx-auto h-16 w-16 text-gray-300 mb-6" />
                <h3 class="text-xl font-semibold text-gray-900 mb-2">No apps running</h3>
                <p class="text-gray-600">Start some apps to see them listed here</p>
              </div>
            <% end %>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("search", %{"value" => search_term}, socket) do
    IO.inspect(search_term, label: "Search term")
    filtered_apps = filter_apps(socket.assigns.apps, search_term, socket.assigns.selected_group)
    {:noreply, assign(socket, search_term: search_term, filtered_apps: filtered_apps)}
  end

  def handle_event("filter_group", %{"value" => group}, socket) do
    IO.inspect(group, label: "Filter group")
    filtered_apps = filter_apps(socket.assigns.apps, socket.assigns.search_term, group)
    {:noreply, assign(socket, selected_group: group, filtered_apps: filtered_apps)}
  end

  @impl true
  def handle_info({type, _app} = event, socket)
      when type in [:app_created, :app_updated, :app_closed] do
    updated_apps = LivebookWeb.AppComponents.update_app_list(socket.assigns.apps, event)

    filtered_apps =
      filter_apps(updated_apps, socket.assigns.search_term, socket.assigns.selected_group)

    {:noreply, assign(socket, apps: updated_apps, filtered_apps: filtered_apps)}
  end

  def handle_info({:server_authorization_updated, _}, socket) do
    apps = Livebook.Apps.list_authorized_apps(socket.assigns.current_user)
    filtered_apps = filter_apps(apps, socket.assigns.search_term, socket.assigns.selected_group)
    {:noreply, assign(socket, apps: apps, filtered_apps: filtered_apps)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp filter_apps(apps, search_term, selected_group) do
    apps
    |> filter_by_search(search_term)
    |> filter_by_group(selected_group)
  end

  defp filter_by_search(apps, ""), do: apps

  defp filter_by_search(apps, search_term) do
    search_term = String.downcase(search_term)

    Enum.filter(apps, fn app ->
      String.contains?(String.downcase(app.notebook_name), search_term)
    end)
  end

  defp filter_by_group(apps, "all"), do: apps

  defp filter_by_group(apps, group) do
    Enum.filter(apps, &(get_app_category(&1) == group))
  end

  defp get_app_groups(apps) do
    apps
    |> Enum.map(&get_app_category/1)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp get_app_category(app) do
    # Try to get the actual group from app settings
    case app.app_spec do
      %Livebook.Apps.PathAppSpec{path: path} when is_binary(path) ->
        case get_notebook_group_from_path(path) do
          nil -> "General"
          "" -> "General"
          group -> String.trim(group)
        end

      %Livebook.Apps.PreviewAppSpec{session_id: session_id} ->
        case get_group_from_preview_session(session_id) do
          nil -> "General"
          "" -> "General"
          group -> String.trim(group)
        end

      _ ->
        "General"
    end
  end

  defp get_group_from_preview_session(session_id) do
    try do
      case Livebook.Sessions.fetch_session(session_id) do
        {:ok, session} ->
          data = Livebook.Session.get_data(session.pid)
          data.notebook.app_settings.group

        :error ->
          nil
      end
    rescue
      _ -> nil
    end
  end

  defp get_notebook_group_from_path(path) do
    if File.exists?(path) do
      try do
        content = File.read!(path)

        case Livebook.LiveMarkdown.notebook_from_livemd(content) do
          {notebook, _} -> notebook.app_settings.group
          _ -> nil
        end
      rescue
        _ -> nil
      end
    else
      nil
    end
  end

  defp group_apps(apps, "all") do
    apps
    |> Enum.group_by(&get_app_category/1)
    |> Enum.sort_by(fn {group, _} -> String.downcase(group) end)
  end

  defp group_apps(apps, selected_group) do
    [{selected_group, apps}]
  end

  defp generate_demo_apps(existing_apps) do
    demo_names = [
      "Customer Analytics Dashboard",
      "Payment Processing Utility",
      "Data Migration Tool",
      "Inventory Management System",
      "User Behavior Analytics",
      "API Rate Limiter Service",
      "Email Campaign Automation",
      "Database Backup Utility",
      "Report Generation Dashboard",
      "File Processing Pipeline",
      "Order Management System",
      "Customer Support Analytics",
      "Data Validation Service",
      "Webhook Processing Tool",
      "Performance Monitoring Dashboard",
      "Content Management Utility",
      "Sales Forecasting Analytics",
      "Authentication Service",
      "Notification Automation",
      "Backup Verification Tool"
    ]

    demo_apps =
      Enum.with_index(demo_names, length(existing_apps) + 1)
      |> Enum.map(fn {name, index} ->
        %{
          notebook_name: name,
          slug: "demo-app-#{index}",
          # Some apps are private
          public?: rem(index, 3) != 0,
          permanent: true,
          pid: nil,
          version: 1,
          warnings: [],
          sessions: [],
          multi_session: false,
          app_spec: %{slug: "demo-app-#{index}"}
        }
      end)

    existing_apps ++ demo_apps
  end
end
