defmodule Livebook.Secrets.Secret do
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          name: String.t() | nil,
          value: String.t() | nil,
          hub_id: String.t() | nil,
          deployment_group_id: String.t() | nil
        }

  @primary_key {:name, :string, autogenerate: false}
  embedded_schema do
    field :value, :string
    field :hub_id, :string
    field :deployment_group_id, :string
  end

  def changeset(secret, attrs \\ %{}) do
    secret
    |> cast(attrs, [:name, :value, :hub_id, :deployment_group_id])
    |> update_change(:name, &String.upcase/1)
    |> validate_format(:name, ~r/^\w+$/,
      message: "should contain only alphanumeric characters and underscore"
    )
    |> validate_required([:name, :value])
  end
end
