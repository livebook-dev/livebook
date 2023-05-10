defmodule Livebook.Secrets.Secret do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          name: String.t() | nil,
          value: String.t() | nil,
          hub_id: String.t() | nil,
          readonly: boolean() | nil
        }

  @primary_key {:name, :string, autogenerate: false}
  embedded_schema do
    field :value, :string
    field :hub_id, :string
    field :readonly, :boolean, virtual: true, default: false
  end

  def changeset(secret, attrs \\ %{}) do
    secret
    |> cast(attrs, [:name, :value, :hub_id])
    |> update_change(:name, &String.upcase/1)
    |> validate_format(:name, ~r/^\w+$/,
      message: "should contain only alphanumeric characters and underscore"
    )
    |> validate_required([:name, :value])
    |> put_readonly()
  end

  defp put_readonly(changeset) do
    case get_field(changeset, :hub_id) do
      "enterprise" <> _ -> put_change(changeset, :readonly, true)
      _any -> changeset
    end
  end
end
