defmodule Livebook.Hubs.Enterprise do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Hubs

  @type t :: %__MODULE__{
          id: String.t() | nil,
          url: String.t() | nil,
          token: String.t() | nil,
          external_id: String.t() | nil,
          hub_name: String.t() | nil,
          hub_color: String.t() | nil
        }

  embedded_schema do
    field :url, :string
    field :token, :string
    field :external_id, :string
    field :hub_name, :string
    field :hub_color, Livebook.EctoTypes.HexColor
  end

  @fields ~w(
    url
    token
    external_id
    hub_name
    hub_color
  )a

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking hub changes.
  """
  @spec change_hub(t(), map()) :: Ecto.Changeset.t()
  def change_hub(%__MODULE__{} = enterprise, attrs \\ %{}) do
    enterprise
    |> changeset(attrs)
    |> Map.put(:action, :validate)
  end

  @doc """
  Creates a Hub.

  With success, notifies interested processes about hub metadatas data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec create_hub(t(), map()) :: {:ok, t()} | {:error, Ecto.Changeset.t()}
  def create_hub(%__MODULE__{} = enterprise, attrs) do
    changeset = changeset(enterprise, attrs)
    id = get_field(changeset, :id)

    if Hubs.hub_exists?(id) do
      {:error,
       changeset
       |> add_error(:external_id, "already exists")
       |> Map.replace!(:action, :validate)}
    else
      with {:ok, struct} <- apply_action(changeset, :insert) do
        Hubs.save_hub(struct)
        {:ok, struct}
      end
    end
  end

  @doc """
  Updates a Hub.

  With success, notifies interested processes about hub metadatas data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec update_hub(t(), map()) :: {:ok, t()} | {:error, Ecto.Changeset.t()}
  def update_hub(%__MODULE__{} = enterprise, attrs) do
    changeset = changeset(enterprise, attrs)
    id = get_field(changeset, :id)

    if Hubs.hub_exists?(id) do
      with {:ok, struct} <- apply_action(changeset, :update) do
        Hubs.save_hub(struct)
        {:ok, struct}
      end
    else
      {:error,
       changeset
       |> add_error(:external_id, "does not exists")
       |> Map.replace!(:action, :validate)}
    end
  end

  defp changeset(enterprise, attrs) do
    enterprise
    |> cast(attrs, @fields)
    |> validate_required(@fields)
    |> add_id()
  end

  defp add_id(changeset) do
    case get_field(changeset, :external_id) do
      nil -> changeset
      external_id -> put_change(changeset, :id, "enterprise-#{external_id}")
    end
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Enterprise do
  def load(%Livebook.Hubs.Enterprise{} = enterprise, fields) do
    %{
      enterprise
      | id: fields.id,
        url: fields.url,
        token: fields.token,
        external_id: fields.external_id,
        hub_name: fields.hub_name,
        hub_color: fields.hub_color
    }
  end

  def normalize(%Livebook.Hubs.Enterprise{} = enterprise) do
    %Livebook.Hubs.Metadata{
      id: enterprise.id,
      name: enterprise.hub_name,
      provider: enterprise,
      color: enterprise.hub_color
    }
  end

  def type(_enterprise), do: "enterprise"

  def connectable?(_enterprise), do: true

  def connect(%Livebook.Hubs.Enterprise{} = enterprise) do
    case Livebook.Hubs.fetch_connected_hub(enterprise) do
      %{pid: pid} -> pid
      nil -> Livebook.Hubs.supervise_hub({Livebook.Hubs.EnterpriseClient, enterprise})
    end
  end
end
