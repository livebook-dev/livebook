defmodule Livebook.Teams.AuthorizationGroup do
  use Ecto.Schema

  @type t :: %__MODULE__{
          provider_id: String.t() | nil,
          group_name: String.t() | nil,
          app_folder_ids: list(String.t())
        }

  @primary_key false
  embedded_schema do
    field :provider_id, :string
    field :group_name, :string
    field :app_folder_ids, {:array, :string}
  end
end
