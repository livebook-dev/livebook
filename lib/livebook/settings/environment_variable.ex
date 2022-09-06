defmodule Livebook.Settings.EnvironmentVariable do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          key: String.t(),
          value: String.t()
        }

  embedded_schema do
    field :key, :string
    field :value, :string
  end

  def changeset(env_var, attrs \\ %{}) do
    env_var
    |> cast(attrs, [:key, :value])
    |> validate_format(:key, ~r/^(?![l|L][b|B]_)\w+$/)
    |> validate_required([:key, :value])
    |> upcase_key()
    |> add_id()
  end

  defp upcase_key(changeset) do
    case get_field(changeset, :key) do
      nil -> changeset
      key -> put_change(changeset, :key, String.upcase(key))
    end
  end

  defp add_id(changeset) do
    case get_field(changeset, :id) do
      nil -> put_change(changeset, :id, Livebook.Utils.random_short_id())
      _ -> changeset
    end
  end
end
