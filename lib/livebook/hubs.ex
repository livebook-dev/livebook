defmodule Livebook.Hubs do
  @moduledoc false

  alias Livebook.Storage
  alias Livebook.Hubs.{Enterprise, Fly, Local, Metadata, Provider}

  @namespace :hubs

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
    :ok = broadcast_hubs_change()
    struct
  end

  @doc false
  def delete_hub(id) do
    Storage.delete(@namespace, id)
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

  @doc """
  Notifies interested processes about hubs data change.

  Broadcasts `{:hubs_metadata_changed, hubs}` message under the `"hubs"` topic.
  """
  @spec broadcast_hubs_change() :: :ok
  def broadcast_hubs_change do
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
end
