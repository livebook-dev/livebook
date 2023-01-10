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
  @spec fetch_hubs() :: list(Provider.t())
  def fetch_hubs do
    for fields <- Storage.all(@namespace) do
      to_struct(fields)
    end
  end

  @doc """
  Gets a list of metadatas from storage.
  """
  @spec fetch_metadatas() :: list(Metadata.t())
  def fetch_metadatas do
    for hub <- fetch_hubs() do
      Provider.normalize(hub)
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
    :ok = broadcast_hubs_change()

    struct
  end

  @doc false
  def delete_hub(id) do
    if hub_exists?(id) do
      hub = fetch_hub!(id)

      if connected_hub = get_connected_hub(hub) do
        GenServer.stop(connected_hub.pid)
      end

      :ok = Storage.delete(@namespace, id)
      :ok = broadcast_hubs_change()
    end

    :ok
  end

  @doc false
  def clean_hubs do
    for hub <- fetch_hubs(), do: delete_hub(hub.id)

    :ok
  end

  @doc """
  Subscribes to updates in hubs information.

  ## Messages

    * `{:hubs_metadata_changed, hubs}`

  """
  @spec subscribe() :: :ok | {:error, term()}
  def subscribe do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "hubs")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe() :: :ok
  def unsubscribe do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "hubs")
  end

  # Notifies interested processes about hubs data change.
  # Broadcasts `{:hubs_metadata_changed, hubs}` message under the `"hubs"` topic.
  defp broadcast_hubs_change do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "hubs", {:hubs_metadata_changed, fetch_metadatas()})
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
    for hub <- fetch_hubs(), do: connect_hub(hub)

    :ok
  end

  @doc """
  Connects to one connectable hub.

  ## Example

      iex> connect_hub(%Enterprise{})
      :ok

  """
  @spec connect_hub(Provider.t()) :: :ok
  def connect_hub(hub) do
    IO.inspect(hub)

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
    for hub <- fetch_hubs(), reduce: [] do
      acc ->
        case get_connected_hub(hub) do
          nil -> acc
          connected_hub -> [connected_hub | acc]
        end
    end
    |> Enum.reverse()
  end

  @doc """
  Gets one connected hub.

  ## Examples

      iex> get_connected_hub(%Enterprise{})
      %{pid: #PID<0.178.0>, hub: %Enterprise{}}

      iex> get_connected_hub(%Enterprise{})
      nil

      iex> get_connected_hub(%Fly{})
      nil

  """
  @spec get_connected_hub(Provider.t()) :: connected_hub() | nil
  def get_connected_hub(hub) do
    case Registry.lookup(Livebook.HubsRegistry, hub.id) do
      [{pid, _}] -> %{pid: pid, hub: hub}
      [] -> nil
    end
  end
end
