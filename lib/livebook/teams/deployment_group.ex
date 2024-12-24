defmodule Livebook.Teams.DeploymentGroup do
  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Secrets.Secret
  alias Livebook.Teams.{AgentKey, EnvironmentVariable}

  @type t :: %__MODULE__{
          id: String.t() | nil,
          name: String.t() | nil,
          url: String.t() | nil,
          mode: :online | :offline,
          clustering: :auto | :dns | nil,
          hub_id: String.t() | nil,
          teams_auth: boolean(),
          secrets: Ecto.Schema.has_many(Secret.t()),
          agent_keys: Ecto.Schema.has_many(AgentKey.t()),
          environment_variables: Ecto.Schema.has_many(EnvironmentVariable.t()),
          zta_provider:
            :basic_auth
            | :cloudflare
            | :google_iap
            | :livebook_teams
            | :tailscale
            | nil
        }

  # TODO: Update this list to be only `:livebook_teams` in the future.
  @zta_providers [:basic_auth, :cloudflare, :google_iap, :livebook_teams, :tailscale]

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :name, :string
    field :mode, Ecto.Enum, values: [:online, :offline], default: :online
    field :hub_id, :string
    field :clustering, Ecto.Enum, values: [:auto, :dns]
    field :zta_provider, Ecto.Enum, values: @zta_providers, default: :livebook_teams
    field :url, :string
    field :teams_auth, :boolean, default: true

    has_many :secrets, Secret
    has_many :agent_keys, AgentKey
    has_many :environment_variables, EnvironmentVariable
  end

  def changeset(deployment_group, attrs \\ %{}) do
    changeset =
      deployment_group
      |> cast(attrs, [:id, :name, :mode, :hub_id, :clustering, :zta_provider, :url, :teams_auth])
      |> validate_required([:name, :mode])
      |> update_change(:url, fn url ->
        if url do
          String.trim_trailing(url, "/")
        end
      end)
      |> validate_change(:url, fn :url, url ->
        case URI.new(url) do
          {:ok, uri} ->
            cond do
              uri.scheme not in ["http", "https"] ->
                [url: ~s(must start with "http://" or "https://")]

              uri.host in ["", nil] ->
                [url: "must be a well-formed URL"]

              true ->
                []
            end

          {:error, _} ->
            [url: "must be a well-formed URL"]
        end
      end)

    if get_field(changeset, :mode) == :offline do
      delete_change(changeset, :zta_provider)
    else
      changeset
    end
  end

  def url_without_scheme(%__MODULE__{url: url} = _deployment_group) do
    case url do
      "http://" <> url -> url
      "https://" <> url -> url
      url -> url
    end
  end
end
