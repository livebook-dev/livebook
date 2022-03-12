defmodule LivebookWeb.SettingsLive.FileSystemsComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <%= for {file_system_id, file_system} <- @file_systems do %>
          <div class="flex items-center justify-between border border-gray-200 rounded-lg p-4">
            <div class="flex items-center space-x-12">
              <.file_system_info file_system={file_system} />
            </div>
            <%= unless is_struct(file_system, FileSystem.Local) do %>
              <button class="button-base button-outlined-red"
                phx-click={
                  with_confirm(
                    JS.push("detach_file_system", value: %{id: file_system_id}),
                    title: "Detach file system",
                    description: "Are you sure you want to detach this file system? Any sessions using it will keep the access until they get closed.",
                    confirm_text: "Detach",
                    confirm_icon: "close-circle-line"
                  )
                }>
                Detach
              </button>
            <% end %>
          </div>
        <% end %>
      </div>
      <div class="flex">
        <%= live_patch "Add file system",
              to: Routes.settings_path(@socket, :add_file_system),
              class: "button-base button-blue" %>
      </div>
    </div>
    """
  end

  defp file_system_info(%{file_system: %FileSystem.Local{}} = assigns) do
    ~H"""
    <.labeled_text label="Type" text="Local disk" />
    """
  end

  defp file_system_info(%{file_system: %FileSystem.S3{}} = assigns) do
    ~H"""
    <.labeled_text label="Type" text="S3" />
    <.labeled_text label="Bucket URL" text={@file_system.bucket_url} />
    """
  end
end
