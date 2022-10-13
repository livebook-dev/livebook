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
            <.file_system_actions
              file_system_id={file_system_id}
              default_file_system_id={@default_file_system_id}
            />
          </div>
        <% end %>
      </div>
      <div class="flex">
        <%= live_patch("Add file system",
          to: Routes.settings_path(@socket, :add_file_system),
          class: "button-base button-blue"
        ) %>
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
      <%= if @default_file_system_id == @file_system_id do %>
        <span class="inline-flex items-center font-sans rounded-full px-2.5 py-0.5 text-xs font-medium bg-gray-100 bg-gray-100 text-gray-800">
          Default
        </span>
      <% end %>
      <%= if @default_file_system_id != @file_system_id or @file_system_id != "local" do %>
        <.menu id={"file-system-#{@file_system_id}-menu"}>
          <:toggle>
            <button class="icon-button" aria-label="open file system menu" type="button">
              <.remix_icon icon="more-2-fill" class="text-xl" />
            </button>
          </:toggle>
          <:content>
            <%= if @default_file_system_id != @file_system_id do %>
              <button
                type="button"
                role="menuitem"
                class="menu-item text-gray-600"
                phx-click="make_default_file_system"
                phx-value-id={@file_system_id}
              >
                <.remix_icon icon="star-line" />
                <span class="font-medium">Make default</span>
              </button>
            <% end %>
            <%= if @file_system_id != "local" do %>
              <button
                type="button"
                role="menuitem"
                class="menu-item text-red-600"
                phx-click={
                  with_confirm(
                    JS.push("detach_file_system", value: %{id: @file_system_id}),
                    title: "Detach file system",
                    description:
                      "Are you sure you want to detach this file system? Any sessions using it will keep the access until they get closed.",
                    confirm_text: "Detach",
                    confirm_icon: "close-circle-line"
                  )
                }
              >
                <.remix_icon icon="delete-bin-line" />
                <span class="font-medium">Detach</span>
              </button>
            <% end %>
          </:content>
        </.menu>
      <% end %>
    </div>
    """
  end
end
