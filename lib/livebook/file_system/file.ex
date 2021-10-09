defmodule Livebook.FileSystem.File do
  @moduledoc false

  # A file points to a specific location in the given
  # file system.
  #
  # This module provides a number of high-level functions
  # similar to the `File` and `Path` core module. Many
  # functions simply delegate the work to the underlying
  # file system.

  defstruct [:file_system, :path]

  alias Livebook.FileSystem

  @type t :: %__MODULE__{
          file_system: FileSystem.t(),
          path: FileSystem.path()
        }

  @doc """
  Builds a new file struct.

  If no path is given, the default file system one is used.
  """
  @spec new(FileSystem.t(), FileSystem.path() | nil) :: t()
  def new(file_system, path \\ nil) do
    default_path = FileSystem.default_path(file_system)

    path =
      if path do
        resolved_path = FileSystem.resolve_path(file_system, default_path, path)

        unless path == resolved_path do
          raise ArgumentError, "expected an expanded absolute path, got: #{inspect(path)}"
        end

        path
      else
        default_path
      end

    %__MODULE__{file_system: file_system, path: path}
  end

  @doc """
  Returns a new file within the `Livebook.FileSystem.Local`
  file system.
  """
  @spec local(FileSystem.path()) :: t()
  def local(path) do
    new(FileSystem.Local.new(), path)
  end

  @doc """
  Returns a new file resulting from resolving `subject`
  against `file`.

  An absolute path may be given, in which case it
  replaces the file path altogether.
  """
  @spec resolve(t(), String.t()) :: t()
  def resolve(file, subject) do
    dir = if dir?(file), do: file, else: containing_dir(file)
    path = FileSystem.resolve_path(file.file_system, dir.path, subject)
    new(file.file_system, path)
  end

  @doc """
  Checks if the given file is a directory.

  Note: this check relies solely on the file path.
  """
  @spec dir?(t()) :: boolean()
  def dir?(file) do
    FileSystem.Utils.dir_path?(file.path)
  end

  @doc """
  Checks if the given file is a regular file.

  Note: this check relies solely on the file path.
  """
  @spec regular?(t()) :: boolean()
  def regular?(file) do
    FileSystem.Utils.regular_path?(file.path)
  end

  @doc """
  Returns file name.
  """
  @spec name(t()) :: String.t()
  def name(file) do
    Path.basename(file.path)
  end

  @doc """
  Returns a directory that contains the given file.

  If a directory is given, the parent directory is returned.
  Root directory is mapped to itself for consistency.
  """
  @spec containing_dir(t()) :: t()
  def containing_dir(file) do
    parent_path =
      if file.path == "/" do
        "/"
      else
        file.path
        |> String.trim_trailing("/")
        |> Path.dirname()
        |> FileSystem.Utils.ensure_dir_path()
      end

    new(file.file_system, parent_path)
  end

  @doc """
  Returns a list of files located in the given directory.

  ## Options

    * `:recursive` - whether to traverse all nested directories,
      defaults to `false`
  """
  @spec list(t(), keyword()) :: {:ok, list(t())} | {:error, FileSystem.error()}
  def list(file, opts \\ []) do
    recursive = Keyword.get(opts, :recursive, false)

    with {:ok, paths} <- FileSystem.list(file.file_system, file.path, recursive) do
      files = for path <- paths, do: new(file.file_system, path)
      {:ok, files}
    end
  end

  @doc """
  Returns binary content of the given file.
  """
  @spec read(t()) :: {:ok, binary()} | {:error, FileSystem.error()}
  def read(file) do
    FileSystem.read(file.file_system, file.path)
  end

  @doc """
  Writes the given binary content to the given file.
  """
  @spec write(t(), binary()) :: :ok | {:error, FileSystem.error()}
  def write(file, content) do
    FileSystem.write(file.file_system, file.path, content)
  end

  @doc """
  Returns the current access level to the given file.
  """
  @spec access(t()) :: {:ok, FileSystem.access()} | {:error, FileSystem.error()}
  def access(file) do
    FileSystem.access(file.file_system, file.path)
  end

  @doc """
  Creates the given directory unless it already exists.
  """
  @spec create_dir(t()) :: :ok | {:error, FileSystem.error()}
  def create_dir(file) do
    FileSystem.create_dir(file.file_system, file.path)
  end

  @doc """
  Removes the given file.
  """
  @spec remove(t()) :: :ok | {:error, FileSystem.error()}
  def remove(file) do
    FileSystem.remove(file.file_system, file.path)
  end

  @doc """
  Copies the given file or directory contents.

  Files from different file systems are supported,
  however keep in mind that this involves reading
  contents of individual files from one file system
  and writing them to the other.
  """
  @spec copy(t(), t()) :: :ok | {:error, FileSystem.error()}
  def copy(source, destination)

  def copy(%{file_system: file_system} = source, %{file_system: file_system} = destination) do
    FileSystem.copy(file_system, source.path, destination.path)
  end

  def copy(source, destination) do
    FileSystem.Utils.assert_same_type!(source.path, destination.path)

    if dir?(source) do
      with {:ok, child_files} <- list(source, recursive: true) do
        child_files
        |> Enum.filter(&regular?/1)
        |> Enum.reduce(:ok, fn child_file, result ->
          with :ok <- result do
            path_sufix = String.replace_leading(child_file.path, source.path, "")
            child_destination = resolve(destination, path_sufix)
            copy_regular_file(child_file, child_destination)
          end
        end)
      end
    else
      copy_regular_file(source, destination)
    end
  end

  defp copy_regular_file(source, destination) do
    with {:ok, content} <- read(source) do
      write(destination, content)
    end
  end

  @doc """
  Renames the given file.

  Files from different file systems are supported,
  however keep in mind that this involves reading
  contents of individual files from one file system
  and writing them to the other.
  """
  @spec rename(t(), t()) :: :ok | {:error, FileSystem.error()}
  def rename(source, destination)

  def rename(%{file_system: file_system} = source, %{file_system: file_system} = destination) do
    FileSystem.rename(file_system, source.path, destination.path)
  end

  def rename(source, destination) do
    FileSystem.Utils.assert_same_type!(source.path, destination.path)

    with {:ok, destination_exist?} <- exists?(destination) do
      if destination_exist? do
        FileSystem.Utils.posix_error(:eexist)
      else
        with :ok <- copy(source, destination) do
          remove(source)
        end
      end
    end
  end

  @doc """
  Returns a version identifier for the given file.
  """
  @spec etag_for(t()) :: {:ok, String.t()} | {:error, FileSystem.error()}
  def etag_for(file) do
    FileSystem.etag_for(file.file_system, file.path)
  end

  @doc """
  Checks if the given file exists.
  """
  @spec exists?(t()) :: {:ok, boolean()} | {:error, FileSystem.error()}
  def exists?(file) do
    FileSystem.exists?(file.file_system, file.path)
  end
end
