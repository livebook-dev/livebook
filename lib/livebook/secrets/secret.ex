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
    field :origin, SecretOrigin
  end

  def changeset(secret, attrs \\ %{}) do
    secret
    |> cast(attrs, [:name, :value, :origin])
    |> update_change(:name, &String.upcase/1)
    |> maybe_put_origin()
    |> validate_format(:name, ~r/^\w+$/,
      message: "should contain only alphanumeric characters and underscore"
    )
    |> validate_required([:name, :value, :origin])
  end

  defp maybe_put_origin(changeset) do
    case get_field(changeset, :origin) do
      nil -> put_change(changeset, :origin, :system_env)
      _ -> changeset
    end
  end
end
