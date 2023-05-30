defmodule Livebook.Hubs.Team do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          id: String.t() | nil,
          org_id: pos_integer() | nil,
          user_id: pos_integer() | nil,
          org_key_id: pos_integer() | nil,
          teams_key: String.t() | nil,
          session_token: String.t() | nil,
          hub_name: String.t() | nil,
          hub_emoji: String.t() | nil
        }

  embedded_schema do
    field :org_id, :integer
    field :user_id, :integer
    field :org_key_id, :integer
    field :teams_key, :string
    field :session_token, :string
    field :hub_name, :string
    field :hub_emoji, :string
  end

  @fields ~w(
    org_id
    user_id
    org_key_id
    teams_key
    session_token
    hub_name
    hub_emoji
  )a

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking hub changes.
  """
  @spec change_hub(t(), map()) :: Ecto.Changeset.t()
  def change_hub(%__MODULE__{} = team, attrs \\ %{}) do
    changeset(team, attrs)
  end

  defp changeset(team, attrs) do
    team
    |> cast(attrs, @fields)
    |> validate_required(@fields)
    |> add_id()
  end

  defp add_id(changeset) do
    if name = get_field(changeset, :hub_name) do
      change(changeset, %{id: "team-#{name}"})
    else
      changeset
    end
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Team do
  alias Livebook.Hubs.TeamClient

  def load(team, fields) do
    %{
      team
      | id: fields.id,
        session_token: fields.session_token,
        teams_key: fields.teams_key,
        org_id: fields.org_id,
        user_id: fields.user_id,
        org_key_id: fields.org_key_id,
        hub_name: fields.hub_name,
        hub_emoji: fields.hub_emoji
    }
  end

  def to_metadata(team) do
    %Livebook.Hubs.Metadata{
      id: team.id,
      name: team.hub_name,
      provider: team,
      emoji: team.hub_emoji,
      connected?: TeamClient.connected?(team.id)
    }
  end

  def type(_team), do: "team"

  def connection_spec(team), do: {TeamClient, team}

  def disconnect(team), do: TeamClient.stop(team.id)

  def capabilities(_team), do: ~w(connect)a

  def get_secrets(_team), do: []

  def create_secret(_team, _secret), do: :ok

  def update_secret(_team, _secret), do: :ok

  def delete_secret(_team, _secret), do: :ok

  def connection_error(team) do
    reason = TeamClient.get_connection_error(team.id)
    "Cannot connect to Hub: #{reason}. Will attempt to reconnect automatically..."
  end

  def notebook_stamp(_hub, _notebook_source, _metadata) do
    :skip
  end

  def verify_notebook_stamp(_hub, _notebook_source, _stamp), do: raise("not implemented")
end
