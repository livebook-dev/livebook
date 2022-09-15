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
end
