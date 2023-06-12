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
          org_public_key: String.t() | nil,
          session_token: String.t() | nil,
          hub_name: String.t() | nil,
          hub_emoji: String.t() | nil
        }

  embedded_schema do
    field :org_id, :integer
    field :user_id, :integer
    field :org_key_id, :integer
    field :teams_key, :string
    field :org_public_key, :string
    field :session_token, :string
    field :hub_name, :string
    field :hub_emoji, :string
  end

  @fields ~w(
    org_id
    user_id
    org_key_id
    teams_key
    org_public_key
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
  alias Livebook.Teams

  def load(team, fields) do
    %{
      team
      | id: fields.id,
        session_token: fields.session_token,
        teams_key: fields.teams_key,
        org_public_key: fields.org_public_key,
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

  def capabilities(_team), do: ~w(connect list_secrets create_secret)a

  def get_secrets(team), do: TeamClient.get_secrets(team.id)

  def create_secret(team, secret), do: Teams.create_secret(team, secret)

  def update_secret(_team, _secret), do: :ok

  def delete_secret(_team, _secret), do: :ok

  def connection_error(team) do
    reason = TeamClient.get_connection_error(team.id)
    "Cannot connect to Hub: #{reason}. Will attempt to reconnect automatically..."
  end

  def notebook_stamp(hub, notebook_source, metadata) do
    # We apply authenticated encryption using the shared teams key,
    # just as for the personal hub, but we additionally sign the token
    # with a private organization key stored on the Teams server. We
    # then validate the signature using the corresponding public key.
    #
    # This results in a two factor mechanism, where creating a valid
    # stamp requires access to the shared local key and an authenticated
    # request to the Teams server (which ensures team membership).

    token = Livebook.Stamping.aead_encrypt(metadata, notebook_source, hub.teams_key)

    case Livebook.Teams.org_sign(hub, token) do
      {:ok, token_signature} ->
        stamp = %{"version" => 1, "token" => token, "token_signature" => token_signature}
        {:ok, stamp}

      _ ->
        {:error, "request to Livebook Teams failed"}
    end
  end

  def verify_notebook_stamp(hub, notebook_source, stamp) do
    %{"version" => 1, "token" => token, "token_signature" => token_signature} = stamp

    if Livebook.Stamping.rsa_verify?(token_signature, token, hub.org_public_key) do
      Livebook.Stamping.aead_decrypt(token, notebook_source, hub.teams_key)
    else
      :error
    end
  end
end
