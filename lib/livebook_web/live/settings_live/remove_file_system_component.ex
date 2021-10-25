defmodule LivebookWeb.SettingsLive.RemoveFileSystemComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Detach file system
      </h3>
      <p class="text-gray-700">
        Are you sure you want to detach this file system?
        Any sessions using it will keep the access until
        they get closed.
      </p>
      <div class="mt-8 flex justify-end space-x-2">
        <button class="button-base button-red" phx-click="detach" phx-target={@myself}>
          <.remix_icon icon="close-circle-line" class="align-middle mr-1" />
          Detach
        </button>
        <%= live_patch "Cancel", to: @return_to, class: "button-base button-outlined-gray" %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("detach", %{}, socket) do
    file_systems = Livebook.Config.remove_file_system(socket.assigns.file_system)
    send(self(), {:file_systems_updated, file_systems})
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end
