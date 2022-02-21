defmodule Livebook.Settings do
  @moduledoc false

  # Keeps all Livebook settings that are backed by storage.

  alias Livebook.FileSystem

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
  Returns all known filesystems.
  """
  @spec file_systems() :: [{Filesystem.id(), Filesystem.t()}]
  def file_systems() do
    restored_file_systems =
      storage().all(:filesystem)
      |> Enum.map(fn %{id: fs_id} = raw_fs ->
        {fs_id, storage_to_fs(raw_fs)}
      end)

    [{:local, Livebook.Config.local_filesystem()} | restored_file_systems]
  end

  @doc """
  Saves a new file system to the configured ones.
  """
  @spec save_filesystem(FileSystem.t()) :: FileSystem.id()
  def save_filesystem(%FileSystem.S3{} = file_system) do
    attributes =
      file_system
      |> FileSystem.S3.to_config()
      |> Map.to_list()

    id = generate_filesystem_id()
    :ok = storage().insert(:filesystem, id, [{:type, "s3"} | attributes])

    id
  end

  @doc """
  Removes the given file system from the configured ones.
  """
  @spec remove_file_system(FileSystem.id()) :: :ok
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

  defp generate_filesystem_id() do
    :crypto.strong_rand_bytes(6) |> Base.url_encode64()
  end
end
