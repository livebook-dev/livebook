defmodule LivebookWeb.SettingsLive do
  use LivebookWeb, :live_view

  import LivebookWeb.UserHelpers

  alias LivebookWeb.{SidebarHelpers, PageHelpers}

  @impl true
  def mount(_params, %{"current_user_id" => current_user_id} = session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "users:#{current_user_id}")
    end

    current_user = build_current_user(session, socket)

    file_systems = Livebook.Config.file_systems()

    {:ok, assign(socket, current_user: current_user, file_systems: file_systems)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-grow h-full">
      <SidebarHelpers.sidebar>
        <SidebarHelpers.logo_item socket={@socket} />
        <SidebarHelpers.break_item />
        <SidebarHelpers.link_item
          icon="settings-3-fill"
          label="Settings"
          path={Routes.settings_path(@socket, :page)}
          active={false} />
        <SidebarHelpers.user_item current_user={@current_user} path={Routes.settings_path(@socket, :user)} />
      </SidebarHelpers.sidebar>
      <div class="flex-grow px-6 py-8 overflow-y-auto">
        <div class="max-w-screen-md w-full mx-auto px-4 pb-8 space-y-8">
          <div>
            <PageHelpers.title text="Settings" socket={@socket} />
            <p class="mt-4 text-gray-700">
              Here you can change global Livebook configuration. Keep in mind
              that this configuration is not persisted and gets discarded as
              soon as you stop the application.
            </p>
          </div>
          <!-- File systems configuration -->
          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-semibold">
              File systems
            </h2>
            <%= live_component LivebookWeb.SettingsLive.FileSystemsComponent,
                  file_systems: @file_systems %>
          </div>
        </div>
      </div>
    </div>

    <%= if @live_action == :user do %>
      <%= live_modal LivebookWeb.UserComponent,
            id: "user",
            modal_class: "w-full max-w-sm",
            user: @current_user,
            return_to: Routes.settings_path(@socket, :page) %>
    <% end %>

    <%= if @live_action == :add_file_system do %>
      <%= live_modal LivebookWeb.SettingsLive.AddFileSystemComponent,
            id: "add-file-system",
            modal_class: "w-full max-w-3xl",
            return_to: Routes.settings_path(@socket, :page) %>
    <% end %>

    <%= if @live_action == :detach_file_system do %>
      <%= live_modal LivebookWeb.SettingsLive.RemoveFileSystemComponent,
            id: "detach-file-system",
            modal_class: "w-full max-w-xl",
            file_system: @file_system,
            return_to: Routes.settings_path(@socket, :page) %>
    <% end %>
    """
  end

  @impl true
  def handle_params(%{"file_system_index" => index}, _url, socket) do
    index = String.to_integer(index)
    file_system = Enum.at(socket.assigns.file_systems, index)
    {:noreply, assign(socket, :file_system, file_system)}
  end

  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_info(
        {:user_change, %{id: id} = user},
        %{assigns: %{current_user: %{id: id}}} = socket
      ) do
    {:noreply, assign(socket, :current_user, user)}
  end

  def handle_info({:file_systems_updated, file_systems}, socket) do
    {:noreply, assign(socket, :file_systems, file_systems)}
  end
end
