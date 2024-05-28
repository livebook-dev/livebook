defmodule Livebook.Teams.DeploymentGroup do
  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Secrets.Secret
  alias Livebook.Teams.AgentKey

  @zta_providers Enum.map(Livebook.Config.identity_providers(), & &1.type)

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :name, :string
    field :mode, Ecto.Enum, values: [:online, :offline], default: :online
    field :hub_id, :string
    field :clustering, Ecto.Enum, values: [:fly_io, :dns]
    field :zta_provider, Ecto.Enum, values: @zta_providers
    field :zta_key, :string
    field :url, :string

    has_many :secrets, Secret
    has_many :agent_keys, AgentKey
  end

  def changeset(deployment_group, attrs \\ %{}) do
    changeset =
      deployment_group
      |> cast(attrs, [:id, :name, :mode, :hub_id, :clustering, :zta_provider, :zta_key, :url])
      |> validate_required([:name, :mode])
      |> update_change(:url, fn url ->
        if url do
          url
          |> String.trim_leading("http://")
          |> String.trim_leading("https://")
          |> String.trim_trailing("/")
        end
      end)
      |> validate_format(:url, ~r/(^[^\/\.\s]+(\.[^\/\.\s]+)+)(\/[^\s]+)?$/,
        message: "must be a well-formed URL"
      )

    if get_field(changeset, :zta_provider) do
      validate_required(changeset, [:zta_key])
    else
      changeset
    end
  end
end
