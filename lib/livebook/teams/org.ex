defmodule Livebook.Teams.Org do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          id: pos_integer() | nil,
          name: String.t() | nil,
          teams_key: String.t() | nil,
          user_code: String.t() | nil
        }

  embedded_schema do
    field :name, :string
    field :teams_key, :string
    field :user_code, :string
  end

  @fields ~w(id name teams_key user_code)a

  @doc """
  Generates a new teams key.
  """
  @spec teams_key() :: String.t()
  def teams_key, do: Base.url_encode64(:crypto.strong_rand_bytes(32), padding: false)

  @doc false
  def changeset(org, attrs) do
    org
    |> cast(attrs, @fields)
    |> generate_teams_key()
    |> validate_required(@fields -- [:id])
  end

  defp generate_teams_key(changeset) do
    if get_field(changeset, :teams_key),
      do: changeset,
      else: put_change(changeset, :teams_key, teams_key())
  end
end
