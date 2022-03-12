defmodule Livebook.Settings do
  @moduledoc false

  # Keeps all Livebook settings that are backed by storage.

  alias Livebook.FileSystem

  @doc """
  Returns the current autosave path.
  """
  @spec autosave_path() :: String.t() | nil
  def autosave_path() do
    case storage().fetch_key(:settings, "global", :autosave_path) do
      {:ok, value} -> value
      :error -> default_autosave_path()
    end
  end

  @doc """
  Returns the default autosave path.
  """
  @spec default_autosave_path() :: String.t()
  def default_autosave_path() do
    Path.join(Livebook.Config.data_path(), "autosaved")
  end

  @doc """
  Sets the current autosave path.
  """
  @spec set_autosave_path(String.t()) :: :ok
  def set_autosave_path(autosave_path) do
    storage().insert(:settings, "global", autosave_path: autosave_path)
  end

  @doc """
  Restores the default autosave path.
  """
  @spec reset_autosave_path() :: :ok
  def reset_autosave_path() do
    storage().delete_key(:settings, "global", :autosave_path)
  end

  @doc """
  Returns all known filesystems.
  """
  @spec file_systems() :: list(FileSystem.t())
  def file_systems() do
    [Livebook.Config.local_filesystem() | Enum.map(storage().all(:filesystem), &storage_to_fs/1)]
  end

  @doc """
  Appends a new file system to the configured ones.

  TODO: Refactor to receive settings submission parameters.
  """
  @spec append_file_system(FileSystem.t()) :: :ok
  def append_file_system(%FileSystem.S3{} = file_system) do
    attributes =
      file_system
      |> FileSystem.S3.to_config()
      |> Map.to_list()

    storage().insert(:filesystem, generate_filesystem_id(), [{:type, "s3"} | attributes])
  end

  @doc """
  Removes the given file system from the configured ones.

  TODO: Refactor to receive the filesystem id.
  """
  @spec remove_file_system(FileSystem.t()) :: :ok
  def remove_file_system(file_system) do
    storage().all(:filesystem)
    |> Enum.find(&(storage_to_fs(&1) == file_system))
    |> then(fn %{id: id} -> storage().delete(:filesystem, id) end)
  end

  defp storage() do
    Livebook.Storage.current()
  end

  defp storage_to_fs(%{type: "s3"} = config) do
    case FileSystem.S3.from_config(config) do
      {:ok, fs} -> fs
      {:error, message} -> raise ArgumentError, "invalid S3 filesystem: #{message}"
    end
  end

  defp generate_filesystem_id() do
    :crypto.strong_rand_bytes(6) |> Base.url_encode64()
  end
end
