defmodule Livebook.Hubs.Fly do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Hubs

  @type t :: %__MODULE__{
          id: String.t() | nil,
          access_token: String.t() | nil,
          hub_name: String.t() | nil,
          hub_emoji: String.t() | nil,
          organization_id: String.t() | nil,
          organization_type: String.t() | nil,
          organization_name: String.t() | nil,
          application_id: String.t() | nil
        }

  embedded_schema do
    field :access_token, :string
    field :hub_name, :string
    field :hub_emoji, :string
    field :organization_id, :string
    field :organization_type, :string
    field :organization_name, :string
    field :application_id, :string
  end

  @fields ~w(
    access_token
    hub_name
    hub_emoji
    organization_id
    organization_name
    organization_type
    application_id
  )a

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking hub changes.
  """
  @spec change_hub(t(), map()) :: Ecto.Changeset.t()
  def change_hub(%__MODULE__{} = fly, attrs \\ %{}) do
    fly
    |> changeset(attrs)
    |> Map.put(:action, :validate)
  end

  @doc """
  Creates a Hub.

  With success, notifies interested processes about hub metadatas data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec create_hub(t(), map()) :: {:ok, t()} | {:error, Ecto.Changeset.t()}
  def create_hub(%__MODULE__{} = fly, attrs) do
    changeset = changeset(fly, attrs)

    if Hubs.hub_exists?(fly.id) do
      {:error,
       changeset
       |> add_error(:application_id, "already exists")
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
  def update_hub(%__MODULE__{} = fly, attrs) do
    changeset = changeset(fly, attrs)

    if Hubs.hub_exists?(fly.id) do
      with {:ok, struct} <- apply_action(changeset, :update) do
        Hubs.save_hub(struct)
        {:ok, struct}
      end
    else
      {:error,
       changeset
       |> add_error(:application_id, "does not exists")
       |> Map.replace!(:action, :validate)}
    end
  end

  defp changeset(fly, attrs) do
    fly
    |> cast(attrs, @fields)
    |> validate_required(@fields)
    |> add_id()
  end

  defp add_id(changeset) do
    if application_id = get_field(changeset, :application_id) do
      change(changeset, %{id: "fly-#{application_id}"})
    else
      changeset
    end
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Fly do
  def load(%Livebook.Hubs.Fly{} = fly, fields) do
    %{
      fly
      | id: fields.id,
        access_token: fields.access_token,
        hub_name: fields.hub_name,
        hub_emoji: fields.hub_emoji,
        organization_id: fields.organization_id,
        organization_type: fields.organization_type,
        organization_name: fields.organization_name,
        application_id: fields.application_id
    }
  end

  def normalize(%Livebook.Hubs.Fly{} = fly) do
    %Livebook.Hubs.Metadata{
      id: fly.id,
      name: fly.hub_name,
      provider: fly,
      emoji: fly.hub_emoji
    }
  end

  def type(_fly), do: "fly"

  def connect(_fly), do: nil

  def connected?(_fly), do: false

  def disconnect(_fly), do: :ok
end
