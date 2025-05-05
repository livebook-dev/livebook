defmodule Livebook.Teams.AuthorizationGroup do
  use Ecto.Schema

  @type t :: %__MODULE__{
          provider_id: String.t() | nil,
          group_name: String.t() | nil
        }

  @primary_key false
  embedded_schema do
    field :provider_id, :string
    field :group_name, :string
  end
end
