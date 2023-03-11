defmodule Livebook.Hubs do
  @moduledoc false

  alias Livebook.Storage
  alias Livebook.Hubs.{Broadcasts, Enterprise, Fly, Metadata, Personal, Provider}
  alias Livebook.Secrets.Secret

  @namespace :hubs

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
  Gets a list of hubs from storage with given capabilities.
  """
  @spec get_hubs(Provider.capabilities()) :: list(Provider.t())
  def get_hubs(capabilities) do
    for hub <- get_hubs(),
        capability?(hub, capabilities),
        do: hub
  end

  @doc """
  Gets a list of metadatas from storage.
  """
  @spec get_metadatas() :: list(Metadata.t())
  def get_metadatas do
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
    attributes = struct |> Map.from_struct() |> Map.to_list()
    :ok = Storage.insert(@namespace, struct.id, attributes)
    :ok = connect_hub(struct)
    :ok = Broadcasts.hub_changed()

    struct
  end

  @doc """
  Deletes a hub with the given id.
  """
  @spec delete_hub(String.t()) :: :ok
  def delete_hub(id) do
    with {:ok, hub} <- fetch_hub(id) do
      true = Provider.type(hub) != "personal"
      :ok = Broadcasts.hub_changed()
      :ok = Storage.delete(@namespace, id)
      :ok = disconnect_hub(hub)
    end

    :ok
  end

  defp disconnect_hub(hub) do
    Task.Supervisor.start_child(Livebook.TaskSupervisor, fn ->
      Process.sleep(30_000)
      :ok = Provider.disconnect(hub)
    end)

    :ok
  end

  @doc """
  Subscribes to one or more subtopics in `"hubs"`.

  ## Messages

  Topic `hubs:crud`:

    * `:hub_changed`

  Topic `hubs:connection`:

    * `:hub_connected`
    * `:hub_disconnected`
    * `{:hub_connection_failed, reason}`

  Topic `hubs:secrets`:

    * `{:secret_created, %Secret{}}`
    * `{:secret_updated, %Secret{}}`

  """
  @spec subscribe(atom() | list(atom())) :: :ok | {:error, term()}
  def subscribe(topics) when is_list(topics) do
    for topic <- topics, do: subscribe(topic)

    :ok
  end

  def subscribe(topic) do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "hubs:#{topic}")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe(atom() | list(atom())) :: :ok
  def unsubscribe(topics) when is_list(topics) do
    for topic <- topics, do: unsubscribe(topic)

    :ok
  end

  def unsubscribe(topic) do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "hubs:#{topic}")
  end

  defp to_struct(%{id: "fly-" <> _} = fields) do
    Provider.load(%Fly{}, fields)
  end

  defp to_struct(%{id: "enterprise-" <> _} = fields) do
    Provider.load(%Enterprise{}, fields)
  end

  defp to_struct(%{id: "personal-" <> _} = fields) do
    Provider.load(%Personal{}, fields)
  end

  @doc """
  Connects to the all available and connectable hubs.

  ## Example

      iex> connect_hubs()
      :ok

  """
  @spec connect_hubs() :: :ok
  def connect_hubs do
    for hub <- get_hubs([:connect]), do: connect_hub(hub)

    :ok
  end

  defp connect_hub(hub) do
    if child_spec = Provider.connection_spec(hub) do
      DynamicSupervisor.start_child(Livebook.HubsSupervisor, child_spec)
    end

    :ok
  end

  @doc """
  Gets a list of hub secrets.

  It gets from all hubs with secret management.
  """
  @spec get_secrets() :: list(Secret.t())
  def get_secrets do
    for hub <- get_hubs([:list_secrets]),
        secret <- Provider.get_secrets(hub),
        do: secret
  end

  @doc """
  Gets a list of secrets for given hub.
  """
  @spec get_secrets(Provider.t()) :: list(Secret.t())
  def get_secrets(hub) do
    if capability?(hub, [:list_secrets]) do
      Provider.get_secrets(hub)
    else
      []
    end
  end

  @doc """
  Creates a secret for given hub.
  """
  @spec create_secret(Provider.t(), Secret.t()) ::
          :ok | {:error, list({atom(), list(String.t())})}
  def create_secret(hub, %Secret{} = secret) do
    true = capability?(hub, [:create_secret])

    Provider.create_secret(hub, secret)
  end

  @doc """
  Updates a secret for given hub.
  """
  @spec update_secret(Provider.t(), Secret.t()) ::
          :ok | {:error, list({atom(), list(String.t())})}
  def update_secret(hub, %Secret{readonly: false} = secret) do
    Provider.update_secret(hub, secret)
  end

  @doc """
  Deletes a secret for given hub.
  """
  @spec delete_secret(Provider.t(), Secret.t()) ::
          :ok | {:error, list({atom(), list(String.t())})}
  def delete_secret(hub, %Secret{readonly: false} = secret) do
    Provider.delete_secret(hub, secret)
  end

  @doc """
  Generates a notebook stamp.
  """
  @spec notebook_stamp(Provider.t(), iodata(), map()) ::
          {:ok, Provider.notebook_stamp()} | :skip | :error
  def notebook_stamp(hub, notebook_source, metadata) do
    Provider.notebook_stamp(hub, notebook_source, metadata)
  end

  @doc """
  Verifies a notebook stamp and returns the decrypted metadata.
  """
  @spec verify_notebook_stamp(Provider.t(), iodata(), Provider.notebook_stamp()) ::
          {:ok, metadata :: map()} | :error
  def verify_notebook_stamp(hub, notebook_source, stamp) do
    Provider.verify_notebook_stamp(hub, notebook_source, stamp)
  end

  @doc """
  Checks the hub capability for given hub.
  """
  @spec capability?(Provider.t(), list(atom())) :: boolean()
  def capability?(hub, capabilities) do
    capabilities -- Provider.capabilities(hub) == []
  end
end
