defmodule LivebookWeb.Hub.FileSystemListComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem
  alias Livebook.FileSystems

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@file_systems == []}>
          No file storages here... yet!
        </.no_entries>
        <div
          :for={file_system <- @file_systems}
          class="flex items-center justify-between border border-gray-200 rounded-lg p-4"
        >
          <div class="flex items-center space-x-12">
            <.labeled_text label="Type">{type(file_system)}</.labeled_text>
            <.labeled_text label="Bucket URL">{name(file_system)}</.labeled_text>
          </div>
          <div class="flex items-center space-x-2">
            <.menu id={"hub-file-system-#{file_system.id}-menu"}>
              <:toggle>
                <.icon_button aria-label="open file system menu" type="button">
                  <.remix_icon icon="more-2-fill" />
                </.icon_button>
              </:toggle>
              <.menu_item>
                <.link
                  id={"hub-file-system-#{file_system.id}-edit"}
                  patch={~p"/hub/#{@hub_id}/file-systems/edit/#{file_system.id}"}
                  type="button"
                  role="menuitem"
                >
                  <.remix_icon icon="file-edit-line" />
                  <span>Edit</span>
                </.link>
              </.menu_item>
              <.menu_item variant="danger">
                <button
                  id={"hub-file-system-#{file_system.id}-detach"}
                  type="button"
                  role="menuitem"
                  class="text-red-600"
                  phx-click={
                    JS.push("detach_file_system",
                      value: %{id: file_system.id, name: name(file_system)}
                    )
                  }
                  phx-target={@myself}
                >
                  <.remix_icon icon="delete-bin-line" />
                  <span>Detach</span>
                </button>
              </.menu_item>
            </.menu>
          </div>
        </div>
      </div>
      <div class="flex">
        <.button
          patch={~p"/hub/#{@hub_id}/file-systems/new"}
          id="add-file-system"
          disabled={@disabled}
        >
          Add file storage
        </.button>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("detach_file_system", %{"id" => id, "name" => name}, socket) do
    on_confirm = fn socket ->
      hub = Livebook.Hubs.fetch_hub!(socket.assigns.hub.id)
      file_systems = Livebook.Hubs.get_file_systems(hub)
      file_system = Enum.find(file_systems, &(&1.id == id))

      case Livebook.Hubs.delete_file_system(hub, file_system) do
        :ok ->
          socket
          |> put_flash(:success, "File storage deleted successfully")
          |> push_patch(to: ~p"/hub/#{hub.id}")

        {:transport_error, reason} ->
          put_flash(socket, :error, reason)
      end
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Detach workspace file storage",
       description: "Are you sure you want to detach #{name}?",
       confirm_text: "Detach",
       confirm_icon: "delete-bin-6-line"
     )}
  end

  defp type(file_system), do: file_system |> FileSystems.type() |> String.upcase()
  defp name(file_system), do: file_system |> FileSystem.external_metadata() |> Map.get(:name)
end
