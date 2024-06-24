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
    field :clustering, Ecto.Enum, values: [:auto, :dns]
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

    if get_field(changeset, :zta_provider) do
      validate_required(changeset, [:zta_key])
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
