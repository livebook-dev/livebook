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
              <%= live_patch "Detach",
                    to: Routes.settings_path(@socket, :detach_file_system, file_system_id),
                    class: "button-base button-outlined-red" %>
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
