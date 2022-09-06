defmodule Livebook.Settings do
  @moduledoc false

  # Keeps all Livebook settings that are backed by storage.

  import Ecto.Changeset, only: [apply_action: 2, add_error: 3]

  alias Livebook.FileSystem
  alias Livebook.Settings.EnvironmentVariable

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
  @spec fetch_env_vars() :: list(EnvironmentVariable.t())
  def fetch_env_vars do
    for fields <- storage().all(:environment_variables) do
      struct!(EnvironmentVariable, fields)
    end
  end

  @doc """
  Gets one environment variable from storage.

  Raises `RuntimeError` if the environment variable does not exist.
  """
  @spec fetch_env_var!(String.t()) :: EnvironmentVariable.t()
  def fetch_env_var!(id) do
    case storage().fetch(:environment_variables, id) do
      :error -> raise RuntimeError, "the environment variable #{id} does not exists in storage"
      {:ok, fields} -> struct!(EnvironmentVariable, fields)
    end
  end

  @doc """
  Checks if environment variable already exists.
  """
  @spec env_var_exists?(String.t()) :: boolean()
  def env_var_exists?(key) do
    Enum.any?(fetch_env_vars(), &(&1.key == key))
  end

  @doc """
  Creates an environment variable from given changeset.

  With success, notifies interested processes about environment variables
  data change. Otherwise, it will return an error tuple with changeset.
  """
  @spec create_env_var(map()) :: {:ok, EnvironmentVariable.t()} | {:error, Ecto.Changeset.t()}
  def create_env_var(attrs \\ %{}) do
    %EnvironmentVariable{}
    |> EnvironmentVariable.changeset(attrs)
    |> apply_action(:insert)
    |> insert_env_var()
  end

  defp insert_env_var({:ok, %EnvironmentVariable{} = env_var}) do
    unless env_var_exists?(env_var.key) do
      save_env_var(env_var)
    else
      {:error,
       env_var
       |> change_env_var()
       |> add_error(:key, "already exists")}
    end
  end

  defp insert_env_var({:error, changeset}), do: {:error, %{changeset | action: :validate}}

  @doc """
  Updates an environment variable from given changeset.

  With success, notifies interested processes about environment variables
  data change. Otherwise, it will return an error tuple with changeset.
  """
  @spec update_env_var(EnvironmentVariable.t(), map()) ::
          {:ok, EnvironmentVariable.t()} | {:error, Ecto.Changeset.t()}
  def update_env_var(%EnvironmentVariable{} = env_var, attrs) do
    env_var
    |> EnvironmentVariable.changeset(attrs)
    |> apply_action(:update)
    |> update_env_var()
  end

  defp update_env_var({:ok, %EnvironmentVariable{} = env_var}) do
    if env_var_exists?(env_var.key) do
      save_env_var(env_var)
    else
      {:error,
       env_var
       |> change_env_var()
       |> add_error(:key, "does not exists")}
    end
  end

  defp update_env_var({:error, changeset}), do: {:error, %{changeset | action: :validate}}

  defp save_env_var(env_var) do
    attributes = env_var |> Map.from_struct() |> Map.to_list()

    with :ok <- storage().insert(:environment_variables, env_var.id, attributes),
         :ok <- broadcast_environment_variables_change() do
      {:ok, env_var}
    end
  end

  @doc """
  Deletes an environment variable from given id.

  Also, it notifies interested processes about environment variables data change.
  """
  @spec delete_env_var(String.t()) :: :ok
  def delete_env_var(id) do
    storage().delete(:environment_variables, id)
    broadcast_environment_variables_change()
  end

  def clean_env_vars do
    for env_var <- fetch_env_vars(), do: delete_env_var(env_var.id)
    :ok
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking environment variable changes.
  """
  @spec change_env_var(EnvironmentVariable.t(), map()) :: Ecto.Changeset.t()
  def change_env_var(%EnvironmentVariable{} = env_var, attrs \\ %{}) do
    env_var
    |> EnvironmentVariable.changeset(attrs)
    |> Map.put(:action, :validate)
  end

  @doc """
  Subscribes to updates in environment variables information.

  ## Messages

    * `{:environment_variables_changed, environment_variables}`

  """
  @spec subscribe() :: :ok | {:error, term()}
  def subscribe do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "environment_variables")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe() :: :ok
  def unsubscribe do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "environment_variables")
  end

  @doc """
  Notifies interested processes about environment variables data change.

  Broadcasts `{:environment_variables_changed, environment_variables}` message under the `"environment_variables"` topic.
  """
  @spec broadcast_environment_variables_change() :: :ok
  def broadcast_environment_variables_change do
    Phoenix.PubSub.broadcast(
      Livebook.PubSub,
      "environment_variables",
      {:environment_variables_changed, fetch_env_vars()}
    )
  end
end
