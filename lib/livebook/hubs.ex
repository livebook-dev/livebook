defmodule Livebook.Hubs do
  @moduledoc false

  alias Livebook.Storage
  alias Livebook.Hubs.{Fly, Hub, Provider}

  defmodule NotFoundError do
    @moduledoc false

    defexception [:id, plug_status: 404]

    def message(%{id: id, type: "fly"}) do
      "could not find a fly entry matching #{inspect(id)}"
    end

    def message(%{id: "fly-" <> id}) do
      message(%{id: id, type: "fly"})
    end
  end

  @namespace :hubs

  @doc """
  Gets a list of hubs from storage.
  """
  @spec fetch_hubs() :: list(Hub.t())
  def fetch_hubs do
    for fields <- Storage.current().all(@namespace) do
      fields
      |> to_struct()
      |> Provider.to_hub()
    end
  end

  @doc """
  Gets one hub from storage.

  Raises `NotFoundError` if the hub does not exist.
  """
  @spec fetch_fly!(String.t()) :: Fly.t()
  def fetch_fly!("fly-" <> _ = id) do
    case Storage.current().fetch(@namespace, id) do
      :error -> raise NotFoundError, id: id, type: "fly"
      {:ok, fields} -> to_struct(fields)
    end
  end

  @doc """
  Gets one provider from storage.

  Raises `NotFoundError` if does not exist.
  """
  @spec provider_by_id!(String.t()) :: String.t()
  def provider_by_id!(id) do
    case Storage.current().fetch(@namespace, id) do
      :error -> raise NotFoundError, id: id
      {:ok, _} -> id_to_provider(id)
    end
  end

  defp id_to_provider("fly-" <> _), do: "fly"

  @doc """
  Checks if Fly already exists.
  """
  @spec fly_exists?(Fly.Organization.t()) :: boolean()
  def fly_exists?(%Fly.Organization{id: id}) do
    case Storage.current().fetch(@namespace, "fly-" <> id) do
      :error -> false
      {:ok, _} -> true
    end
  end

  @doc """
  Saves a new hub to the configured ones.
  """
  @spec save_fly(Fly.t()) :: Fly.t()
  def save_fly(%Fly{organization: organization} = fly) do
    id = "fly-" <> fly.id

    attributes =
      fly
      |> Map.delete(:id)
      |> Map.replace!(:organization, to_ets(organization))
      |> to_ets()

    with :ok <- Storage.current().insert(@namespace, id, attributes),
         :ok <- broadcast_hubs_change() do
      fly
    end
  end

  @doc false
  def delete_entry(id) do
    Storage.current().delete(@namespace, id)
  end

  @doc false
  def clean_hubs do
    for hub <- fetch_hubs(), do: delete_entry(hub.id)

    :ok
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

  defp to_ets(data) when is_struct(data) do
    data |> Map.from_struct() |> to_ets()
  end

  defp to_ets(data) when is_map(data) do
    Map.to_list(data)
  end

  defp to_struct(%{id: "fly-" <> id, organization: organization} = fields) do
    fields =
      fields
      |> Map.replace!(:id, id)
      |> Map.replace!(:organization, struct!(Fly.Organization, organization))

    struct!(Fly, fields)
  end
end
