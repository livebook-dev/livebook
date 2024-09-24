defmodule Livebook.Teams.AgentKey do
  use Ecto.Schema

  @type t :: %__MODULE__{
          id: String.t() | nil,
          key: String.t() | nil,
          deployment_group_id: String.t() | nil
        }

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :key, :string
    field :deployment_group_id, :string
  end
end
