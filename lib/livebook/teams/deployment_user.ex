defmodule Livebook.Teams.DeploymentUser do
  use Ecto.Schema

  @type t :: %__MODULE__{
          user_id: String.t() | nil,
          deployment_group_id: String.t() | nil
        }

  @primary_key false
  embedded_schema do
    field :user_id, :string
    field :deployment_group_id, :string
  end
end
