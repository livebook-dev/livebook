defmodule Livebook.Secrets.Secret do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.EctoTypes.SecretOrigin

  @type t :: %__MODULE__{
          name: String.t(),
          value: String.t(),
          origin: SecretOrigin.t()
        }

  @primary_key {:name, :string, autogenerate: false}
  embedded_schema do
    field :value, :string
    field :origin, SecretOrigin, default: :app
  end

  def changeset(secret, attrs \\ %{}) do
    secret
    |> cast(attrs, [:name, :value, :origin])
    |> update_change(:name, &String.upcase/1)
    |> validate_format(:name, ~r/^\w+$/,
      message: "should contain only alphanumeric characters and underscore"
    )
    |> validate_required([:name, :value, :origin])
  end
end
