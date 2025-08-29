defmodule LivebookWeb.FileSystemComponents do
  use LivebookWeb, :html

  alias Livebook.FileSystem

  @doc """
  Formats the given file system into a short name.
  """
  def file_system_name(file_system_module)

  def file_system_name(FileSystem.Local), do: "Disk"
  def file_system_name(FileSystem.S3), do: "S3"
  def file_system_name(FileSystem.Git), do: "Git"

  @doc """
  Formats the given file system into a descriptive label.
  """
  def file_system_label(file_system)

  def file_system_label(%FileSystem.Local{}), do: "Disk"
  def file_system_label(%FileSystem.S3{} = fs), do: fs.bucket_url
  def file_system_label(%FileSystem.Git{} = fs), do: fs.repo_url

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

  def file_system_icon(%{file_system: %FileSystem.Git{}} = assigns) do
    ~H"""
    <.remix_icon icon="git-repository-line leading-none" />
    """
  end
end
