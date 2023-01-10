defmodule Livebook.Hubs do
  @moduledoc false

  alias Livebook.Storage
  alias Livebook.Hubs.{Enterprise, Fly, Local, Metadata, Provider}

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
  Gets a list of metadatas from storage.
  """
  @spec get_metadatas() :: list(Metadata.t())
  def get_metadatas do
    for hub <- get_hubs() do
      metadata = Provider.normalize(hub)

      if Provider.connect(hub),
        do: %{metadata | connected?: hub_connected?(hub)},
        else: metadata
    end
  end

  @doc """
  Gets one hub from storage.
  """
  @spec get_hub(String.t()) :: {:ok, Provider.t()} | :error
  def get_hub(id) do
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
    :ok = broadcast_message(:crud, {:hubs_metadata_changed, get_metadatas()})

    struct
  end

  @doc false
  def delete_hub(id) do
    with {:ok, hub} <- get_hub(id) do
      if connected_hub = get_connected_hub(hub) do
        GenServer.stop(connected_hub.pid, :shutdown)
      end

      :ok = Storage.delete(@namespace, id)
      :ok = broadcast_message(:crud, {:hubs_metadata_changed, get_metadatas()})
    end

    :ok
  end

  @doc false
  def clean_hubs do
    for hub <- get_hubs(), do: delete_hub(hub.id)

    :ok
  end

  @doc """
  Subscribes to one or more subtopics in `"hubs"`.

  ## Messages

  Topic `hubs:crud`:

    * `{:hubs_metadata_changed, hubs}`

  Topic `hubs:connection`:

    * `{:connect, :ok, :connected}`
    * `{:connect, :error, reason}`
    * `{:disconnect, :error, reason}`

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

  @doc """
  Notifies interested processes about hubs.

  Broadcasts the message under given subtopic inside `"hubs"` topic.
  """
  @spec broadcast_message(atom(), any()) :: :ok
  def broadcast_message(topic, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "hubs:#{topic}", message)
  end

  defp to_struct(%{id: "fly-" <> _} = fields) do
    Provider.load(%Fly{}, fields)
  end

  defp to_struct(%{id: "enterprise-" <> _} = fields) do
    Provider.load(%Enterprise{}, fields)
  end

  defp to_struct(%{id: "local-" <> _} = fields) do
    Provider.load(%Local{}, fields)
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
    if child_spec = Provider.connect(hub) do
      DynamicSupervisor.start_child(Livebook.HubsSupervisor, child_spec)
    end

    :ok
  end

  @doc """
  Gets a list of connected hubs.

  ## Example

      iex> get_connected_hubs()
      [%{pid: #PID<0.178.0>, hub: %Enterprise{}}, ...]

  """
  @spec get_connected_hubs() :: connected_hubs()
  def get_connected_hubs do
    for hub <- get_hubs(), connected = get_connected_hub(hub), do: connected
  end

  defp get_connected_hub(hub) do
    case Registry.lookup(Livebook.HubsRegistry, hub.id) do
      [{pid, _}] -> %{pid: pid, hub: hub}
      [] -> nil
    end
  end

  defp hub_connected?(hub) do
    if connected_hub = get_connected_hub(hub) do
      GenServer.call(connected_hub.pid, :connected?)
    else
      false
    end
  end
end
