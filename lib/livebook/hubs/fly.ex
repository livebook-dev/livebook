defmodule Livebook.Hubs.Fly do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.{Hubs, Utils}

  @type t :: %__MODULE__{
          id: Livebook.Utils.id(),
          access_token: String.t(),
          hub_name: String.t(),
          hub_color: String.t(),
          organization_id: String.t(),
          organization_type: String.t(),
          organization_name: String.t(),
          application_id: String.t()
        }

  embedded_schema do
    field(:access_token, :string)
    field(:hub_name, :string)
    field(:hub_color, :string)
    field(:organization_id, :string)
    field(:organization_type, :string)
    field(:organization_name, :string)
    field(:application_id, :string)
  end

  @fields ~w(
    access_token
    hub_name
    hub_color
    organization_id
    organization_name
    organization_type
    application_id
  )a

  def changeset(fly, attrs \\ %{}) do
    fly
    |> cast(attrs, @fields)
    |> validate_required(@fields)
    |> validate_color()
    |> add_id()
  end

  defp validate_color(changeset) do
    case get_field(changeset, :hub_color) do
      nil ->
        changeset

      hex_color ->
        if Utils.valid_hex_color?(hex_color) do
          changeset
        else
          add_error(changeset, :hub_color, "not a valid color")
        end
    end
  end

  defp add_id(changeset) do
    if application_id = get_field(changeset, :application_id) do
      change(changeset, %{id: "fly-#{application_id}"})
    else
      validate_required(changeset, [:id])
    end
  end

  def create_fly(%__MODULE__{} = fly, attrs) do
    changeset = changeset(fly, attrs)

    if Hubs.hub_exists?(fly.id) do
      {:error, add_error(changeset, :application_id, "already exists")}
    else
      with {:ok, struct} <- apply_action(changeset, :insert) do
        Hubs.save_hub(struct)
        {:ok, struct}
      end
    end
  end

  def update_fly(%__MODULE__{} = fly, attrs) do
    changeset = changeset(fly, attrs)

    if Hubs.hub_exists?(fly.id) do
      with {:ok, struct} <- apply_action(changeset, :update) do
        Hubs.save_hub(struct)
        {:ok, struct}
      end
    else
      {:error, add_error(changeset, :application_id, "does not exists")}
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
        hub_color: fields.hub_color,
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
      color: fly.hub_color
    }
  end

  def type(_), do: "fly"
end
