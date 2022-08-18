defmodule Livebook.Hubs do
  @moduledoc false

  alias Livebook.Storage
  alias Livebook.Hubs.{Fly, Metadata, Provider}

  defmodule NotFoundError do
    @moduledoc false

    defexception [:id, plug_status: 404]

    def message(%{id: id}) do
      "could not find a hub matching #{inspect(id)}"
    end
  end

  @namespace :hubs

  @doc """
  Gets a list of hubs from storage.
  """
  @spec fetch_hubs() :: list(Provider.t())
  def fetch_hubs do
    for fields <- Storage.current().all(@namespace) do
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

  Raises `NotFoundError` if the hub does not exist.
  """
  @spec fetch_hub!(String.t()) :: Provider.t()
  def fetch_hub!(id) do
    case Storage.current().fetch(@namespace, id) do
      :error -> raise NotFoundError, id: id
      {:ok, fields} -> to_struct(fields)
    end
  end

  @doc """
  Checks if hub already exists.
  """
  @spec hub_exists?(String.t()) :: boolean()
  def hub_exists?(id) do
    case Storage.current().fetch(@namespace, id) do
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

    with :ok <- Storage.current().insert(@namespace, struct.id, attributes),
         :ok <- broadcast_hubs_change() do
      struct
    end
  end

  @doc false
  def delete_hub(id) do
    Storage.current().delete(@namespace, id)
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
end
