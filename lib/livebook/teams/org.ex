defmodule Livebook.Teams.Org do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  @prefix "lb_tk_"

  @type t :: %__MODULE__{
          id: pos_integer() | nil,
          emoji: String.t() | nil,
          name: String.t() | nil,
          teams_key: String.t() | nil,
          user_code: String.t() | nil
        }

  @secret_key_size 32

  @primary_key {:id, :id, autogenerate: false}
  embedded_schema do
    field :emoji, :string
    field :name, :string
    field :teams_key, :string
    field :user_code, :string
  end

  @fields ~w(id emoji name teams_key user_code)a
  @required_fields @fields -- ~w(id user_code)a

  @doc """
  Generates a new teams key.
  """
  @spec teams_key() :: String.t()
  def teams_key() do
    key = :crypto.strong_rand_bytes(@secret_key_size)
    @prefix <> Base.url_encode64(key, padding: false)
  end

  @doc """
  Generates a hash key.
  """
  @spec key_hash(t()) :: String.t()
  def key_hash(%__MODULE__{teams_key: teams_key}),
    do: Base.url_encode64(:crypto.hash(:sha256, teams_key), padding: false)

  @doc false
  def changeset(org, attrs) do
    org
    |> cast(attrs, @fields)
    |> validate_required(@required_fields)
    |> validate_format(:name, ~r/^[a-z0-9][a-z0-9\-]*$/,
      message: "should only contain lowercase alphanumeric characters and dashes"
    )
  end

  @doc """
  Returns the teams key prefix
  """
  @spec teams_key_prefix() :: String.t()
  def teams_key_prefix(), do: @prefix
end
