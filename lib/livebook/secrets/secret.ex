defmodule Livebook.Secrets.Secret do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          name: String.t(),
          value: String.t()
        }

  @primary_key {:name, :string, autogenerate: false}
  embedded_schema do
    field :value, :string
  end

  def changeset(secret, attrs \\ %{}) do
    secret
    |> cast(attrs, [:name, :value])
    |> update_change(:name, &String.upcase/1)
    |> validate_format(:name, ~r/^\w+$/,
      message: "should contain only alphanumeric and underscore"
    )
    |> validate_required([:name, :value])
  end
end
