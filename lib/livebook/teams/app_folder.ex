defmodule Livebook.Teams.AppFolder do
  use Ecto.Schema

  @type t :: %__MODULE__{
          id: String.t() | nil,
          name: String.t() | nil,
          hub_id: String.t() | nil
        }

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :name, :string
    field :hub_id, :string
  end
end
