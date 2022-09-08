defmodule Livebook.Settings.EnvVar do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          key: String.t(),
          value: String.t()
        }

  @primary_key {:key, :string, autogenerate: false}
  embedded_schema do
    field :value, :string
  end

  def changeset(env_var, attrs \\ %{}) do
    env_var
    |> cast(attrs, [:key, :value])
    |> update_change(:key, &String.upcase/1)
    |> validate_format(:key, ~r/^(?!LB_)\w+$/)
    |> validate_required([:key, :value])
  end
end
