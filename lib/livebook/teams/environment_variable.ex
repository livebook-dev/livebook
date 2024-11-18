defmodule Livebook.Teams.EnvironmentVariable do
  use Ecto.Schema

  @type t :: %__MODULE__{
          name: String.t(),
          value: String.t(),
          hub_id: String.t(),
          deployment_group_id: String.t()
        }

  @enforce_keys [:name, :value, :hub_id, :deployment_group_id]

  @primary_key false
  embedded_schema do
    field :name, :string
    field :value, :string
    field :hub_id, :string
    field :deployment_group_id, :string
  end
end
