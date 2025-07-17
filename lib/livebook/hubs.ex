defmodule Livebook.Hubs do
  alias Livebook.FileSystem
  alias Livebook.Storage
  alias Livebook.Hubs
  alias Livebook.Hubs.Provider
  alias Livebook.Secrets.Secret

  require Logger

  @namespace :hubs
  @supervisor Livebook.HubsSupervisor

  @type connected_hub :: %{
          required(:pid) => pid(),
          required(:hub) => Provider.t()
        }
  @type connected_hubs :: list(connected_hub())

  @doc """
  Gets a list of hubs from storage.
  """
  @spec get_hubs() :: list(Provider.t())
  def get_hubs do
    for fields <- Storage.all(@namespace) do
      to_struct(fields)
    end
  end

  @doc """
  Gets a list of metadata from storage.
  """
  @spec get_metadata() :: list(Hubs.Metadata.t())
  def get_metadata do
    for hub <- get_hubs() do
      Provider.to_metadata(hub)
    end
  end

  @doc """
  Gets one hub from storage.
  """
  @spec fetch_hub(String.t()) :: {:ok, Provider.t()} | :error
  def fetch_hub(id) do
    with {:ok, data} <- Storage.fetch(@namespace, id) do
      {:ok, to_struct(data)}
    end
  end

  @doc """
  Gets one hub from storage.

  Raises `Livebook.Storage.NotFoundError` if the hub does not exist.
  """
  @spec fetch_hub!(String.t()) :: Provider.t()
  def fetch_hub!(id) do
    Storage.fetch!(@namespace, id) |> to_struct()
  end

  @doc """
  Checks if hub already exists.
  """
  @spec hub_exists?(String.t()) :: boolean()
  def hub_exists?(id) do
    case Storage.fetch(@namespace, id) do
      :error -> false
      {:ok, _} -> true
    end
  end

  @doc """
  Saves a new hub to the configured ones.
  """
  @spec save_hub(Provider.t()) :: Provider.t()
  def save_hub(struct) do
    attributes = Provider.dump(struct)
    :ok = connect_hub(struct)
    :ok = Storage.insert(@namespace, struct.id, Map.to_list(attributes))
    :ok = Hubs.Broadcasts.hub_changed(struct.id)

    struct
  end

  @doc """
  Deletes a hub with the given id.
  """
  @spec delete_hub(String.t()) :: :ok
  def delete_hub(id) do
    with {:ok, hub} <- fetch_hub(id) do
      true = Provider.type(hub) != "personal"
      :ok = maybe_unset_default_hub(hub.id)
      :ok = Storage.delete(@namespace, id)
      :ok = Hubs.Broadcasts.hub_deleted(hub.id)
      :ok = disconnect_hub(hub)
    end

    :ok
  end

  @spec set_default_hub(String.t()) :: :ok
  def set_default_hub(id) do
    with {:ok, hub} <- fetch_hub(id) do
      :ok = Storage.insert(:default_hub, "default_hub", [{:default_hub, hub.id}])
    end

    :ok
  end

  @spec unset_default_hub() :: :ok
  def unset_default_hub() do
    :ok = Storage.delete(:default_hub, "default_hub")
  end

  @spec get_default_hub() :: Provider.t()
  def get_default_hub() do
    with {:ok, %{default_hub: id}} <- Storage.fetch(:default_hub, "default_hub"),
         {:ok, hub} <- fetch_hub(id) do
      hub
    else
      _ -> fetch_hub!(Hubs.Personal.id())
    end
  end

  defp maybe_unset_default_hub(hub_id) do
    if get_default_hub().id == hub_id, do: unset_default_hub(), else: :ok
  end

  defp disconnect_hub(hub) do
    # We use a task supervisor because the hub connection itself
    # calls delete_hub (which calls this function), otherwise we deadlock.
    Task.Supervisor.start_child(Livebook.TaskSupervisor, fn ->
      # Since other processes may have been communicating
      # with the hub, we don't want to terminate abruptly and
      # make them crash, so we give it some time to shut down.
      #
      # The default backoff is 5.5s, so we round it down to 5s.
      Process.sleep(30_000)
      :ok = Provider.disconnect(hub)
    end)

    :ok
  end

  defp to_struct(%{id: "personal-" <> _} = fields) do
    Provider.load(%Hubs.Personal{}, fields)
  end

  defp to_struct(%{id: "team-" <> _} = fields) do
    Provider.load(Hubs.Team.new(), fields)
  end

  @doc """
  Connects to the all available and connectable hubs.

  ## Example

      iex> connect_hubs()
      :ok

  """
  @spec connect_hubs() :: :ok
  def connect_hubs do
    for hub <- get_hubs(), do: connect_hub(hub)

    :ok
  end

  defp connect_hub(hub) do
    if child_spec = Provider.connection_spec(hub) do
      case DynamicSupervisor.start_child(@supervisor, child_spec) do
        {:ok, _} ->
          :ok

        {:error, {:already_started, _pid}} ->
          :ok

        {:error, reason} ->
          Logger.error("Could not start Workspace #{hub.id}: #{Exception.format_exit(reason)}")
      end
    end

    :ok
  end

  @doc """
  Gets a list of hub secrets.

  It gets from all hubs with secret management.
  """
  @spec get_secrets() :: list(Secret.t())
  def get_secrets do
    for hub <- get_hubs(),
        secret <- Provider.get_secrets(hub),
        do: secret
  end

  @doc """
  Gets a list of secrets for given hub.
  """
  @spec get_secrets(Provider.t()) :: list(Secret.t())
  def get_secrets(hub) do
    hub
    |> Provider.get_secrets()
    |> Enum.sort()
  end

  @doc """
  Creates a secret for given hub.
  """
  @spec create_secret(Provider.t(), Secret.t()) ::
          :ok
          | {:error, Provider.field_errors()}
          | {:transport_error, String.t()}
  def create_secret(hub, %Secret{} = secret) do
    Provider.create_secret(hub, secret)
  end

  @doc """
  Updates a secret for given hub.
  """
  @spec update_secret(Provider.t(), Secret.t()) ::
          :ok
          | {:error, Provider.field_errors()}
          | {:transport_error, String.t()}
  def update_secret(hub, %Secret{} = secret) do
    Provider.update_secret(hub, secret)
  end

  @doc """
  Deletes a secret for given hub.
  """
  @spec delete_secret(Provider.t(), Secret.t()) :: :ok | {:transport_error, String.t()}
  def delete_secret(hub, %Secret{} = secret) do
    Provider.delete_secret(hub, secret)
  end

  @doc """
  Generates a notebook stamp.
  """
  @spec notebook_stamp(Provider.t(), iodata(), map()) ::
          {:ok, Provider.notebook_stamp()} | :skip | {:error, String.t()}
  def notebook_stamp(hub, notebook_source, metadata) do
    Provider.notebook_stamp(hub, notebook_source, metadata)
  end

  @doc """
  Verifies a notebook stamp and returns the decrypted metadata.
  """
  @spec verify_notebook_stamp(Provider.t(), iodata(), Provider.notebook_stamp()) ::
          {:ok, metadata :: map()} | {:error, :invalid | :too_recent_version}
  def verify_notebook_stamp(hub, notebook_source, stamp) do
    Provider.verify_notebook_stamp(hub, notebook_source, stamp)
  end

  @doc """
  Gets a list of file systems from all hubs.
  """
  @spec get_file_systems() :: list(FileSystem.t())
  def get_file_systems() do
    file_systems = Enum.flat_map(get_hubs(), &Provider.get_file_systems/1)
    local_file_system = Livebook.Config.local_file_system()

    [local_file_system | Enum.sort_by(file_systems, & &1.id)]
  end

  @doc """
  Gets a list of file systems for given hub.
  """
  @spec get_file_systems(Provider.t(), keyword()) :: list(FileSystem.t())
  def get_file_systems(hub, opts \\ []) do
    hub_file_systems = Provider.get_file_systems(hub)
    sorted_hub_file_systems = Enum.sort_by(hub_file_systems, & &1.id)

    if opts[:hub_only],
      do: sorted_hub_file_systems,
      else: [Livebook.Config.local_file_system() | sorted_hub_file_systems]
  end

  @doc """
  Creates a file system for given hub.
  """
  @spec create_file_system(Provider.t(), FileSystem.t()) ::
          :ok
          | {:error, Provider.field_errors()}
          | {:transport_error, String.t()}
  def create_file_system(hub, file_system) do
    Provider.create_file_system(hub, file_system)
  end

  @doc """
  Updates a file system for given hub.
  """
  @spec update_file_system(Provider.t(), FileSystem.t()) ::
          :ok
          | {:error, Provider.field_errors()}
          | {:transport_error, String.t()}
  def update_file_system(hub, file_system) do
    Provider.update_file_system(hub, file_system)
  end

  @doc """
  Deletes a file system for given hub.
  """
  @spec delete_file_system(Provider.t(), FileSystem.t()) :: :ok | {:transport_error, String.t()}
  def delete_file_system(hub, file_system) do
    Provider.delete_file_system(hub, file_system)
  end

  @doc """
  Gets a list of hub app specs.
  """
  @spec get_app_specs() :: list(Livebook.Apps.AppSpec.t())
  def get_app_specs() do
    for hub <- get_hubs(),
        Provider.connection_spec(hub),
        app_spec <- Provider.get_app_specs(hub),
        do: app_spec
  end
end
