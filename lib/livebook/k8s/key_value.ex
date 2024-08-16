defmodule Livebook.K8s.KeyValue do
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          key: String.t() | nil,
          value: String.t() | nil
        }

  @primary_key {:key, :string, autogenerate: false}
  embedded_schema do
    field :value, :string
  end

  def changeset(env_var, attrs \\ %{}) do
    env_var
    |> cast(attrs, [:key, :value])
    |> validate_required([:key, :value])
  end
end
