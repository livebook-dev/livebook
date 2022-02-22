defmodule Livebook.Settings do
  @moduledoc false

  # Keeps all Livebook settings that are backed by storage.

  alias Livebook.FileSystem

  @typedoc """
  An id that is used for filesystem's manipulation, either insertion or removal.
  """
  @type file_system_id :: :local | String.t()

  @doc """
  Returns the autosave path.

  TODO: Make this configurable in the UI.
  """
  @spec autosave_path() :: String.t() | nil
  def autosave_path() do
    case storage().fetch_key(:settings, "global", :autosave_path) do
      {:ok, value} -> value
      :error -> Path.join(Livebook.Config.data_path(), "autosaved")
    end
  end

  @doc """
  Returns all known filesystems with their associated ids.

  In case of the local filesystem the id resolves to `:local` atom.
  """
  @spec file_systems() :: [{file_system_id(), Filesystem.t()}]
  def file_systems() do
    restored_file_systems =
      storage().all(:filesystem)
      |> Enum.sort_by(&Map.get(&1, :order, System.os_time()))
      |> Enum.map(fn %{id: fs_id} = raw_fs ->
        {fs_id, storage_to_fs(raw_fs)}
      end)

    [{:local, Livebook.Config.local_filesystem()} | restored_file_systems]
  end

  @doc """
  Saves a new file system to the configured ones.
  """
  @spec save_filesystem(FileSystem.t()) :: file_system_id()
  def save_filesystem(%FileSystem.S3{} = file_system) do
    attributes =
      file_system
      |> FileSystem.S3.to_config()
      |> Map.to_list()

    id = Livebook.Utils.random_short_id()

    :ok =
      storage().insert(:filesystem, id, [{:type, "s3"}, {:order, System.os_time()} | attributes])

    id
  end

  @doc """
  Removes the given file system from the configured ones.
  """
  @spec remove_file_system(file_system_id()) :: :ok
  def remove_file_system(filesystem_id) do
    storage().delete(:filesystem, filesystem_id)
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
end
