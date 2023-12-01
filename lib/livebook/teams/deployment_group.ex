defmodule Livebook.Teams.DeploymentGroup do
  use Ecto.Schema
  import Ecto.Changeset
  alias Livebook.Secrets.Secret

  @type t :: %__MODULE__{
          id: pos_integer() | nil,
          name: String.t() | nil,
          mode: :online | :offline,
          hub_id: String.t() | nil,
          secrets: list()
        }

  @primary_key {:id, :id, autogenerate: false}
  embedded_schema do
    field :name, :string
    field :mode, Ecto.Enum, values: [:online, :offline]
    field :hub_id, :string
    has_many :secrets, Secret
  end

  def changeset(deployment_group, attrs \\ %{}) do
    deployment_group
    |> cast(attrs, [:id, :name, :mode, :hub_id])
    |> validate_required([:name, :mode])
  end
end
