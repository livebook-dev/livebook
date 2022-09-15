defmodule LivebookWeb.FileSystemHelpers do
  use Phoenix.Component

  import LivebookWeb.LiveHelpers

  alias Livebook.FileSystem

  @doc """
  Formats the given file system into a descriptive label.
  """
  def file_system_label(file_system)

  def file_system_label(%FileSystem.Local{}), do: "Local disk"
  def file_system_label(%FileSystem.S3{} = fs), do: fs.bucket_url

  @doc """
  Renders an icon representing the given file system.
  """
  def file_system_icon(assigns)

  def file_system_icon(%{file_system: %FileSystem.Local{}} = assigns) do
    ~H"""
    <.remix_icon icon="hard-drive-2-line leading-none" />
    """
  end

  def file_system_icon(%{file_system: %FileSystem.S3{}} = assigns) do
    ~H"""
    <i class="not-italic">
      <span class="text-[0.75em] font-semibold align-middle">S3</span>
    </i>
    """
  end

  @doc """
  Renders the location of a file, including its file system.
  """
  def file_location_display(assigns) do
    assigns =
      assigns
      |> assign_new(:label, fn -> nil end)
      |> assign_new(:file, fn -> nil end)
      |> assign_new(:class, fn -> "" end)
      |> assign_new(:inner_block, fn -> nil end)
      |> assign_new(:before_location, fn -> nil end)
      |> assign(
        :attrs,
        assigns_to_attributes(assigns, [:label, :file, :class, :inner_block, :before_location])
      )

    ~H"""
    <div class={"flex space-x-2 items-center max-w-full #{@class}"} {@attrs}>
      <%= if @label do %>
        <span class="text-gray-700 whitespace-nowrap"><%= @label %></span>
      <% end %>
      <%= if @before_location do %>
        <%= render_slot(@before_location) %>
      <% end %>
      <%= if @file do %>
        <span class="tooltip right" data-tooltip={file_system_label(@file.file_system)}>
          <span class="flex items-center">
            [<.file_system_icon file_system={@file.file_system} />]
          </span>
        </span>
        <span class="text-gray-700 whitespace-no-wrap font-medium text-ellipsis overflow-hidden">
          <%= @file.path %>
        </span>
      <% else %>
        <span class="text-gray-700 whitespace-no-wrap">
          no file selected
        </span>
      <% end %>
      <%= if @inner_block do %>
        <%= render_slot(@inner_block) %>
      <% end %>
    </div>
    """
  end
end
