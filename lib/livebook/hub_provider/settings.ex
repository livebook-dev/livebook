defmodule Livebook.HubProvider.Settings do
  @moduledoc false

  alias Livebook.Storage
  alias Livebook.HubProvider.Hub

  defmodule NotFoundError do
    @moduledoc false

    defexception [:id, plug_status: 404]

    def message(%{id: id}) do
      "could not find a hub matching #{inspect(id)}"
    end
  end

  @namespace :hub

  @doc """
  Gets a list of hubs from storage.
  """
  @spec fetch_hubs() :: list(Hub.t())
  def fetch_hubs do
    Storage.current().all(@namespace)
  end

  @doc """
  Gets one hub from storage.

  Raises `NotFoundError` if the hub does not exist.
  """
  @spec hub_by_id!(String.t()) :: Hub.t()
  def hub_by_id!(id) do
    case Storage.current().fetch(@namespace, id) do
      :error -> raise NotFoundError, id: id
      {:ok, fields} -> struct!(Hub, fields)
    end
  end

  @doc """
  Checks if hub already exists.
  """
  @spec hub_exists?(Hub.t()) :: boolean()
  def hub_exists?(%Hub{id: id}) do
    case Storage.current().fetch(@namespace, id) do
      :error -> false
      {:ok, _} -> true
    end
  end

  @doc """
  Saves a new hub to the configured ones.
  """
  @spec save_hub(Hub.t()) :: Hub.t()
  def save_hub(hub) do
    attributes =
      hub
      |> Map.from_struct()
      |> Map.to_list()

    :ok = Storage.current().insert(@namespace, hub.id, attributes)

    hub
  end

  @doc """
  Subscribes to updates in hubs information.

  ## Messages

    * `{:hubs_changed, hubs}`

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

  @doc """
  Notifies interested processes about hubs data change.

  Broadcasts `{:hubs_changed, hubs}` message under the `"hubs"` topic.
  """
  @spec broadcast_hubs_change() :: :ok
  def broadcast_hubs_change do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "hubs", {:hubs_changed, fetch_hubs()})
  end
end
