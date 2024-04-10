defmodule Livebook.Teams.DeploymentGroup do
  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Secrets.Secret
  alias Livebook.Teams.AgentKey

  # If this list is updated, it must also be mirrored on Livebook Teams Server.
  @zta_providers ~w(cloudflare google_iap tailscale teleport)a

  @type t :: %__MODULE__{
          id: String.t() | nil,
          name: String.t() | nil,
          mode: :online | :offline,
          hub_id: String.t() | nil,
          clustering: :fly_io | nil,
          zta_provider: :cloudflare | :google_iap | :tailscale | :teleport,
          zta_key: String.t(),
          secrets: [Secret.t()],
          agent_keys: [AgentKey.t()]
        }

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :name, :string
    field :mode, Ecto.Enum, values: [:online, :offline], default: :online
    field :hub_id, :string
    field :clustering, Ecto.Enum, values: [:fly_io]
    field :zta_provider, Ecto.Enum, values: @zta_providers
    field :zta_key, :string

    has_many :secrets, Secret
    has_many :agent_keys, AgentKey
  end

  def changeset(deployment_group, attrs \\ %{}) do
    deployment_group
    |> cast(attrs, [:id, :name, :mode, :hub_id, :clustering, :zta_provider, :zta_key])
    |> validate_required([:name, :mode])
  end
end
