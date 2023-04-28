defmodule Livebook.Teams.Org do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Teams

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
  Creates an Org.

  With success, notifies interested processes about org creation.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec create_org(t(), map()) :: {:ok, map()} | {:error, Ecto.Changeset.t()}
  def create_org(%__MODULE__{} = org, attrs) do
    changeset = changeset(org, attrs)

    case apply_action(changeset, :insert) do
      {:ok, org} -> Teams.create_org(org)
      {:error, changeset} -> {:error, Map.replace!(changeset, :action, :validate)}
    end
  end

  @doc """
  Add errors from given map to given changeset.
  """
  @spec add_errors(t(), map()) :: Ecto.Changeset.t()
  def add_errors(%__MODULE__{} = org, errors_map) do
    for {field, errors} <- errors_map,
        field in __schema__(:fields),
        error <- errors,
        reduce: validate_org(org),
        do: (acc -> add_error(acc, field, error))
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking org changes.
  """
  @spec change_org(t(), map()) :: Ecto.Changeset.t()
  def change_org(%__MODULE__{} = org, attrs \\ %{}) do
    changeset(org, attrs)
  end

  @doc """
  Returns changeset with applied validations.
  """
  @spec validate_org(t(), map()) :: Ecto.Changeset.t()
  def validate_org(%__MODULE__{} = org, attrs \\ %{}) do
    org
    |> changeset(attrs)
    |> Map.put(:action, :validate)
  end

  @doc """
  Generates a new teams key.
  """
  @spec teams_key() :: String.t()
  def teams_key, do: Base.url_encode64(:crypto.strong_rand_bytes(32))

  defp changeset(org, attrs) do
    org
    |> cast(attrs, @fields)
    |> generate_teams_key()
    |> validate_required(@fields)
  end

  defp generate_teams_key(changeset) do
    if get_field(changeset, :teams_key),
      do: changeset,
      else: put_change(changeset, :teams_key, teams_key())
  end
end
