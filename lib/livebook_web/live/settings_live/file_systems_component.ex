defmodule LivebookWeb.SettingsLive.FileSystemsComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <div
          :for={file_system <- @file_systems}
          class="flex items-center justify-between border border-gray-200 rounded-lg p-4"
        >
          <div class="flex items-center space-x-12">
            <.file_system_info file_system={file_system} />
          </div>
          <.file_system_actions file_system_id={file_system.id} />
        </div>
      </div>
      <div class="flex">
        <.link patch={~p"/settings/add-file-system"} class="button-base button-blue">
          Add file system
        </.link>
      </div>
    </div>
    """
  end

  defp file_system_info(%{file_system: %FileSystem.Local{}} = assigns) do
    ~H"""
    <.labeled_text label="Type">Local disk</.labeled_text>
    """
  end

  defp file_system_info(%{file_system: %FileSystem.S3{}} = assigns) do
    ~H"""
    <.labeled_text label="Type">S3</.labeled_text>
    <.labeled_text label="Bucket URL"><%= @file_system.bucket_url %></.labeled_text>
    """
  end

  defp file_system_actions(assigns) do
    ~H"""
    <div class="flex items-center space-x-2">
      <.menu :if={@file_system_id != "local"} id={"file-system-#{@file_system_id}-menu"}>
        <:toggle>
          <button class="icon-button" aria-label="open file system menu" type="button">
            <.remix_icon icon="more-2-fill" class="text-xl" />
          </button>
        </:toggle>
        <.menu_item variant={:danger}>
          <button
            :if={@file_system_id != "local"}
            type="button"
            role="menuitem"
            class="text-red-600"
            phx-click={JS.push("detach_file_system", value: %{id: @file_system_id})}
          >
            <.remix_icon icon="delete-bin-line" />
            <span>Detach</span>
          </button>
        </.menu_item>
      </.menu>
    </div>
    """
  end
end
