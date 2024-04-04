defmodule Livebook.Teams.Agent do
  use Ecto.Schema

  @type t :: %__MODULE__{
          id: String.t() | nil,
          name: String.t() | nil,
          hub_id: String.t() | nil,
          org_id: String.t() | nil,
          deployment_group_id: String.t() | nil
        }

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :name, :string
    field :hub_id, :string
    field :org_id, :string
    field :deployment_group_id, :string
  end
end
