defmodule Livebook.Settings.EnvVar do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          name: String.t() | nil,
          value: String.t() | nil
        }

  @primary_key {:name, :string, autogenerate: false}
  embedded_schema do
    field :value, :string
  end

  def changeset(env_var, attrs \\ %{}) do
    env_var
    |> cast(attrs, [:name, :value])
    |> update_change(:name, &String.upcase/1)
    |> validate_format(:name, ~r/^(?!LB_)\w+$/, message: "cannot start with the LB_ prefix")
    |> validate_required([:name, :value])
  end
end
