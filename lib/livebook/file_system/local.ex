defmodule Livebook.FileSystem.Local do
  @moduledoc false

  # File system backed by local disk.

  defstruct [:default_path]

  alias Livebook.FileSystem

  @type t :: %__MODULE__{
          default_path: FileSystem.path()
        }

  @doc """
  Returns a new file system struct.

  ## Options

    * `:default_path` - the default directory path. Defaults
      to the current working directory
  """
  @spec new(keyword()) :: t()
  def new(opts \\ []) do
    default_path =
      Keyword.get_lazy(opts, :default_path, fn ->
        File.cwd!() |> FileSystem.Utils.ensure_dir_path()
      end)

    FileSystem.Utils.assert_dir_path!(default_path)

    %__MODULE__{default_path: default_path}
  end
end

defimpl Livebook.FileSystem, for: Livebook.FileSystem.Local do
  alias Livebook.FileSystem

  def default_path(file_system) do
    file_system.default_path
  end

  def list(file_system, path, recursive) do
    FileSystem.Utils.assert_dir_path!(path)

    case File.ls(path) do
      {:ok, filenames} ->
        paths =
          Enum.map(filenames, fn name ->
            path = Path.join(path, name)
            if File.dir?(path), do: path <> "/", else: path
          end)

        to_traverse =
          if recursive do
            Enum.filter(paths, &FileSystem.Utils.dir_path?/1)
          else
            []
          end

        Enum.reduce(to_traverse, {:ok, paths}, fn path, result ->
          with {:ok, current_paths} <- result,
               {:ok, new_paths} <- list(file_system, path, recursive) do
            {:ok, current_paths ++ new_paths}
          end
        end)

      {:error, error} ->
        FileSystem.Utils.posix_error(error)
    end
  end

  def read(_file_system, path) do
    FileSystem.Utils.assert_regular_path!(path)

    case File.read(path) do
      {:ok, binary} -> {:ok, binary}
      {:error, error} -> FileSystem.Utils.posix_error(error)
    end
  end

  def write(_file_system, path, content) do
    FileSystem.Utils.assert_regular_path!(path)

    dir = Path.dirname(path)

    with :ok <- File.mkdir_p(dir),
         :ok <- File.write(path, content) do
      :ok
    else
      {:error, error} -> FileSystem.Utils.posix_error(error)
    end
  end

  def access(_file_system, path) do
    case File.stat(path) do
      {:ok, stat} -> {:ok, stat.access}
      {:error, error} -> FileSystem.Utils.posix_error(error)
    end
  end

  def create_dir(_file_system, path) do
    FileSystem.Utils.assert_dir_path!(path)

    case File.mkdir_p(path) do
      :ok -> :ok
      {:error, error} -> FileSystem.Utils.posix_error(error)
    end
  end

  def remove(_file_system, path) do
    case File.rm_rf(path) do
      {:ok, _paths} -> :ok
      {:error, error} -> FileSystem.Utils.posix_error(error)
    end
  end

  def copy(_file_system, source_path, destination_path) do
    FileSystem.Utils.assert_same_type!(source_path, destination_path)

    containing_dir = Path.dirname(destination_path)

    case File.mkdir_p(containing_dir) do
      :ok ->
        case File.cp_r(source_path, destination_path) do
          {:ok, _paths} -> :ok
          {:error, error, _path} -> FileSystem.Utils.posix_error(error)
        end

      {:error, error} ->
        FileSystem.Utils.posix_error(error)
    end
  end

  def rename(_file_system, source_path, destination_path) do
    FileSystem.Utils.assert_same_type!(source_path, destination_path)

    if File.exists?(destination_path) do
      FileSystem.Utils.posix_error(:eexist)
    else
      containing_dir = Path.dirname(destination_path)

      with :ok <- File.mkdir_p(containing_dir),
           :ok <- File.rename(source_path, destination_path) do
        :ok
      else
        {:error, error} ->
          FileSystem.Utils.posix_error(error)
      end
    end
  end

  def etag_for(_file_system, path) do
    case File.stat(path) do
      {:ok, stat} ->
        %{size: size, mtime: mtime} = stat
        hash = {size, mtime} |> :erlang.phash2() |> Integer.to_string(16)
        etag = <<?", hash::binary, ?">>
        {:ok, etag}

      {:error, error} ->
        FileSystem.Utils.posix_error(error)
    end
  end

  def exists?(_file_system, path) do
    if FileSystem.Utils.dir_path?(path) do
      {:ok, File.dir?(path)}
    else
      {:ok, File.exists?(path)}
    end
  end

  def resolve_path(_file_system, dir_path, subject) do
    FileSystem.Utils.assert_dir_path!(dir_path)

    if subject == "" do
      dir_path
    else
      dir? = FileSystem.Utils.dir_path?(subject) or Path.basename(subject) in [".", ".."]
      expanded_path = Path.expand(subject, dir_path)

      if dir? do
        FileSystem.Utils.ensure_dir_path(expanded_path)
      else
        expanded_path
      end
    end
  end
end
