defmodule Livebook.FileSystem.File do
  # A file points to a specific location in the given file system.
  #
  # This module provides a number of high-level functions similar to
  # the `File` and `Path` core module. Many functions simply delegate
  # the work to the underlying file system.

  defstruct [:file_system_id, :file_system_module, :path, :origin_pid]

  alias Livebook.FileSystem

  @type t :: %__MODULE__{
          file_system_id: String.t(),
          file_system_module: module,
          path: FileSystem.path(),
          # We cannot just store the node, because when the struct is
          # built, we may not yet be in distributed mode. Instead, we
          # keep the pid of whatever process created this file system
          # and we call node/1 on it whenever needed
          origin_pid: pid()
        }

  @doc """
  Builds a new file struct.

  If no path is given, the default file system one is used.
  """
  @spec new(FileSystem.t(), FileSystem.path() | nil) :: t()
  def new(%module{} = file_system, path \\ nil) do
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

    %__MODULE__{
      file_system_id: file_system.id,
      file_system_module: module,
      path: path,
      origin_pid: self()
    }
  end

  @doc """
  Returns a new file within the `Livebook.FileSystem.Local` file
  system.
  """
  @spec local(FileSystem.path()) :: t()
  def local(path) do
    new(Livebook.Config.local_file_system(), path)
  end

  @doc """
  Returns a term uniquely identifying the file together with its file
  system.
  """
  @spec resource_identifier(t()) :: term()
  def resource_identifier(file) do
    # Note that file system id should by definition encapsulate
    # information about the underlying resource. We also include node
    # if the file system is node-dependent

    node =
      if FileSystem.type(struct!(file.file_system_module)) == :local do
        node(file.origin_pid)
      end

    {file.file_system_id, node, file.path}
  end

  @doc """
  Checks if two files are equal.

  Comparing files with `Kernel.==/2` may result in false-negatives,
  because the structs hold additional information.
  """
  @spec equal?(t(), t()) :: boolean()
  def equal?(file1, file2) do
    file1.path == file2.path and file1.file_system_id == file2.file_system_id and
      file1.file_system_module == file2.file_system_module
  end

  @doc """
  Checks if the given file is within a file system local to its node.
  """
  @spec local?(t()) :: term()
  def local?(file) do
    FileSystem.type(struct!(file.file_system_module)) == :local
  end

  @doc """
  Returns a new file resulting from resolving `subject` against `file`.

  An absolute path may be given, in which case it replaces the file
  path altogether.
  """
  @spec resolve(t(), String.t()) :: t()
  def resolve(file, subject) do
    dir = if dir?(file), do: file, else: containing_dir(file)

    path = FileSystem.resolve_path(struct!(file.file_system_module), dir.path, subject)

    %{file | path: path}
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

  If a directory is given, the parent directory is returned. Root
  directory is mapped to itself for consistency.
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

    %{file | path: parent_path}
  end

  @doc """
  Returns a list of files located in the given directory.

  ## Options

    * `:recursive` - whether to traverse all nested directories.
      Defaults to `false`

  """
  @spec list(t(), keyword()) :: {:ok, list(t())} | {:error, FileSystem.error()}
  def list(file, opts \\ []) do
    recursive = Keyword.get(opts, :recursive, false)

    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id),
         {:ok, paths} <- FileSystem.list(file_system, file.path, recursive) do
      files = for path <- paths, do: new(file_system, path)
      {:ok, files}
    end
  end

  @doc """
  Returns binary content of the given file.
  """
  @spec read(t()) :: {:ok, binary()} | {:error, FileSystem.error()}
  def read(file) do
    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id) do
      FileSystem.read(file_system, file.path)
    end
  end

  @doc """
  Writes the given binary content to the given file.
  """
  @spec write(t(), binary()) :: :ok | {:error, FileSystem.error()}
  def write(file, content) do
    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id) do
      FileSystem.write(file_system, file.path, content)
    end
  end

  @doc """
  Returns the current access level to the given file.
  """
  @spec access(t()) :: {:ok, FileSystem.access()} | {:error, FileSystem.error()}
  def access(file) do
    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id) do
      FileSystem.access(file_system, file.path)
    end
  end

  @doc """
  Creates the given directory unless it already exists.
  """
  @spec create_dir(t()) :: :ok | {:error, FileSystem.error()}
  def create_dir(file) do
    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id) do
      FileSystem.create_dir(file_system, file.path)
    end
  end

  @doc """
  Removes the given file.
  """
  @spec remove(t()) :: :ok | {:error, FileSystem.error()}
  def remove(file) do
    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id) do
      FileSystem.remove(file_system, file.path)
    end
  end

  @doc """
  Copies the given file or directory contents.

  Files from different file systems are supported, however keep in
  mind that this copies individual files chunk by chunk from one file
  system to the other.
  """
  @spec copy(t(), t()) :: :ok | {:error, FileSystem.error()}
  def copy(source, destination)

  def copy(%{file_system_id: fs_id} = source, %{file_system_id: fs_id} = destination) do
    with :ok <- maybe_ensure_local(source),
         :ok <- maybe_ensure_local(destination),
         {:ok, file_system} <- do_fetch_file_system(fs_id) do
      FileSystem.copy(file_system, source.path, destination.path)
    end
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
    with {:ok, _} <- read_stream_into(source, destination) do
      :ok
    end
  end

  @doc """
  Renames the given file.

  Files from different file systems are supported, however keep in
  mind that this copies individual files chunk by chunk from one file
  system to the other.
  """
  @spec rename(t(), t()) :: :ok | {:error, FileSystem.error()}
  def rename(source, destination)

  def rename(%{file_system_id: fs_id} = source, %{file_system_id: fs_id} = destination) do
    with :ok <- maybe_ensure_local(source),
         :ok <- maybe_ensure_local(destination),
         {:ok, file_system} <- do_fetch_file_system(fs_id) do
      FileSystem.rename(file_system, source.path, destination.path)
    end
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
    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id) do
      FileSystem.etag_for(file_system, file.path)
    end
  end

  @doc """
  Checks if the given file exists.
  """
  @spec exists?(t()) :: {:ok, boolean()} | {:error, FileSystem.error()}
  def exists?(file) do
    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id) do
      FileSystem.exists?(file_system, file.path)
    end
  end

  @doc """
  Adds an extension to the file name, unless already present.
  """
  @spec ensure_extension(t(), String.t()) :: t()
  def ensure_extension(file, "." <> _ = extension) do
    Map.update!(file, :path, fn path ->
      if String.ends_with?(path, extension) do
        path
      else
        path <> extension
      end
    end)
  end

  @doc """
  Similar to `read/2`, but streams file contents into `collectable`
  chunk by chunk.
  """
  @spec read_stream_into(t(), Collectable.t()) ::
          {:ok, Collectable.t()} | {:error, FileSystem.error()}
  def read_stream_into(file, collectable) do
    with :ok <- maybe_ensure_local(file),
         {:ok, file_system} <- do_fetch_file_system(file.file_system_id) do
      FileSystem.read_stream_into(file_system, file.path, collectable)
    end
  end

  @doc """
  Checks if the given files use the same file system.

  For local file systems also checks if both files actually point to
  the same node.
  """
  @spec same_file_system?(t(), t()) :: boolean()
  def same_file_system?(file1, file2)

  def same_file_system?(%{file_system_id: id} = file1, %{file_system_id: id} = file2) do
    case {local?(file1), local?(file2)} do
      {false, false} -> true
      {true, true} -> node(file1.origin_pid) == node(file2.origin_pid)
    end
  end

  def same_file_system?(_file1, _file2), do: false

  @doc """
  Looks up file system that this file uses.

  The file system may not be available in certain cases, for example
  when it has been detached.
  """
  @spec fetch_file_system(t()) :: {:ok, FileSystem.t()} | {:error, FileSystem.error()}
  def fetch_file_system(file) do
    do_fetch_file_system(file.file_system_id)
  end

  defp do_fetch_file_system(file_system_id) do
    file_system = Livebook.Hubs.get_file_systems() |> Enum.find(&(&1.id == file_system_id))

    if file_system do
      {:ok, file_system}
    else
      {:error,
       "could not find file system (id: #{file_system_id}). This means that it has" <>
         " been either detached or cannot be accessed from the Workspace at the moment"}
    end
  end

  @doc false
  def maybe_ensure_local(file) do
    if local?(file) do
      if node(file.origin_pid) == node() do
        :ok
      else
        {:error, "cannot access local file from a different host"}
      end
    else
      :ok
    end
  end
end

defimpl Collectable, for: Livebook.FileSystem.File do
  alias Livebook.FileSystem

  def into(%FileSystem.File{path: path} = file) do
    with :ok <- FileSystem.File.maybe_ensure_local(file),
         {:ok, file_system} <- FileSystem.File.fetch_file_system(file),
         {:ok, state} <- FileSystem.write_stream_init(file_system, path, []) do
      collector = fn
        state, {:cont, chunk} when is_binary(chunk) ->
          case FileSystem.write_stream_chunk(file_system, state, chunk) do
            {:ok, state} ->
              state

            {:error, error} ->
              cancel(file_system, state)
              raise error
          end

        state, :done ->
          case FileSystem.write_stream_finish(file_system, state) do
            :ok ->
              file

            {:error, error} ->
              cancel(file_system, state)
              raise error
          end

        state, :halt ->
          cancel(file_system, state)
          :ok
      end

      {state, collector}
    else
      {:error, error} -> raise error
    end
  end

  defp cancel(file_system, state) do
    # Try to cleanup
    _ = FileSystem.write_stream_halt(file_system, state)
  end
end
