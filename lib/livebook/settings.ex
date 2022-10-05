defmodule Livebook.Settings do
  @moduledoc false

  # Keeps all Livebook settings that are backed by storage.

  import Ecto.Changeset, only: [apply_action: 2]

  alias Livebook.FileSystem
  alias Livebook.Settings.EnvVar

  defmodule NotFoundError do
    @moduledoc false

    defexception [:message, plug_status: 404]
  end

  @typedoc """
  An id that is used for filesystem's manipulation, either insertion or removal.
  """
  @type file_system_id :: :local | String.t()

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

    [{"local", Livebook.Config.local_filesystem()} | restored_file_systems]
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
    if default_file_system_id() == filesystem_id do
      storage().delete_key(:settings, "global", :default_file_system_id)
    end

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

  @doc """
  Returns whether the update check is enabled.
  """
  @spec update_check_enabled?() :: boolean()
  def update_check_enabled?() do
    case storage().fetch_key(:settings, "global", :update_check_enabled) do
      {:ok, value} -> value
      :error -> true
    end
  end

  @doc """
  Sets whether the update check is enabled.
  """
  @spec set_update_check_enabled(boolean()) :: :ok
  def set_update_check_enabled(enabled) do
    storage().insert(:settings, "global", update_check_enabled: enabled)
  end

  @doc """
  Gets a list of environment variables from storage.
  """
  @spec fetch_env_vars() :: list(EnvVar.t())
  def fetch_env_vars do
    for fields <- storage().all(:env_vars) do
      struct!(EnvVar, Map.delete(fields, :id))
    end
  end

  @doc """
  Gets one environment variable from storage.

  Raises `RuntimeError` if the environment variable does not exist.
  """
  @spec fetch_env_var!(String.t()) :: EnvVar.t()
  def fetch_env_var!(id) do
    case storage().fetch(:env_vars, id) do
      :error ->
        raise NotFoundError,
          message: "could not find an environment variable matching #{inspect(id)}"

      {:ok, fields} ->
        struct!(EnvVar, Map.delete(fields, :id))
    end
  end

  @doc """
  Checks if environment variable already exists.
  """
  @spec env_var_exists?(String.t()) :: boolean()
  def env_var_exists?(id) do
    storage().fetch(:env_vars, id) != :error
  end

  @doc """
  Sets the given environment variable.

  With success, notifies interested processes about environment variable
  data change. Otherwise, it will return an error tuple with changeset.
  """
  @spec set_env_var(EnvVar.t(), map()) ::
          {:ok, EnvVar.t()} | {:error, Ecto.Changeset.t()}
  def set_env_var(%EnvVar{} = env_var \\ %EnvVar{}, attrs) do
    changeset = EnvVar.changeset(env_var, attrs)

    with {:ok, env_var} <- apply_action(changeset, :insert) do
      save_env_var(env_var)
    end
  end

  defp save_env_var(env_var) do
    attributes = env_var |> Map.from_struct() |> Map.to_list()

    with :ok <- storage().insert(:env_vars, env_var.name, attributes),
         :ok <- broadcast_env_vars_change({:env_var_set, env_var}) do
      {:ok, env_var}
    end
  end

  @doc """
  Unsets an environment variable from given id.

  With success, notifies interested processes about environment variable
  deletion. Otherwise, it does nothing.
  """
  @spec unset_env_var(String.t()) :: :ok
  def unset_env_var(id) do
    if env_var_exists?(id) do
      env_var = fetch_env_var!(id)
      storage().delete(:env_vars, id)
      broadcast_env_vars_change({:env_var_unset, env_var})
    end

    :ok
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking environment variable changes.
  """
  @spec change_env_var(EnvVar.t(), map()) :: Ecto.Changeset.t()
  def change_env_var(%EnvVar{} = env_var, attrs \\ %{}) do
    env_var
    |> EnvVar.changeset(attrs)
    |> Map.put(:action, :validate)
  end

  @doc """
  Subscribes to updates in settings information.

  ## Messages

    * `{:env_var_set, env_var}`
    * `{:env_var_unset, env_var}`

  """
  @spec subscribe() :: :ok | {:error, term()}
  def subscribe do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "settings")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe() :: :ok
  def unsubscribe do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "settings")
  end

  # Notifies interested processes about environment variables data change.
  #
  # Broadcasts given message under the `"settings"` topic.
  defp broadcast_env_vars_change(message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "settings", message)
  end

  @doc """
  Sets default file system.
  """
  @spec set_default_file_system(file_system_id()) :: :ok
  def set_default_file_system(file_system_id) do
    storage().insert(:settings, "global", default_file_system_id: file_system_id)
  end

  @doc """
  Settings default file system
  """
  def default_file_system do
    case storage().fetch_key(:settings, "global", :default_file_system_id) do
      :error ->
        Livebook.Config.local_filesystem_home()

      {:ok, default_file_system_id} ->
        {_id, file} =
          Enum.find(Livebook.Settings.file_systems(), fn {id, _file} ->
            id == default_file_system_id
          end)

        FileSystem.File.new(file)
    end
  end

  @doc """
  Get default file system id
  """
  def default_file_system_id do
    case storage().fetch_key(:settings, "global", :default_file_system_id) do
      :error ->
        "local"

      {:ok, default_file_system_id} ->
        {id, _file} =
          Enum.find(Livebook.Settings.file_systems(), fn {id, _file} ->
            id == default_file_system_id
          end)

        id
    end
  end
end
