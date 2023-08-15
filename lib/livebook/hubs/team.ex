defmodule Livebook.Hubs.Team do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  defmodule Offline do
    @moduledoc false
    use Ecto.Schema

    alias Livebook.Secrets.Secret

    @type t :: %__MODULE__{
            secrets: list(Secret.t())
          }

    @primary_key false
    embedded_schema do
      field :secrets, {:array, :map}, default: []
    end
  end

  @type t :: %__MODULE__{
          id: String.t() | nil,
          org_id: non_neg_integer() | nil,
          user_id: non_neg_integer() | nil,
          org_key_id: non_neg_integer() | nil,
          teams_key: String.t() | nil,
          org_public_key: String.t() | nil,
          session_token: String.t() | nil,
          hub_name: String.t() | nil,
          hub_emoji: String.t() | nil,
          offline: Offline.t() | nil
        }

  @enforce_keys [:user_id, :org_id, :org_key_id, :session_token, :org_public_key, :teams_key]

  embedded_schema do
    field :org_id, :integer
    field :user_id, :integer
    field :org_key_id, :integer
    field :teams_key, :string
    field :org_public_key, :string
    field :session_token, :string
    field :hub_name, :string
    field :hub_emoji, :string

    embeds_one :offline, Offline
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
  Initializes a new Team hub.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      user_id: nil,
      org_id: nil,
      org_key_id: nil,
      session_token: nil,
      org_public_key: nil,
      teams_key: nil
    }
  end

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

  @doc """
  Returns the public key prefix
  """
  @spec public_key_prefix() :: String.t()
  def public_key_prefix(), do: "lb_opk_"
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Team do
  alias Livebook.Hubs.TeamClient
  alias Livebook.Teams

  @teams_key_prefix Teams.Org.teams_key_prefix()
  @public_key_prefix Livebook.Hubs.Team.public_key_prefix()

  def load(team, fields) do
    struct(team, fields)
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

  def update_secret(team, secret), do: Teams.update_secret(team, secret)

  def delete_secret(team, secret), do: Teams.delete_secret(team, secret)

  def connection_error(team) do
    reason = TeamClient.get_connection_error(team.id)
    "Cannot connect to Hub: #{reason}.\nWill attempt to reconnect automatically..."
  end

  def notebook_stamp(team, notebook_source, metadata) do
    # We apply authenticated encryption using the shared teams key,
    # just as for the personal hub, but we additionally sign the token
    # with a private organization key stored on the Teams server. We
    # then validate the signature using the corresponding public key.
    #
    # This results in a two factor mechanism, where creating a valid
    # stamp requires access to the shared local key and an authenticated
    # request to the Teams server (which ensures team membership).

    @teams_key_prefix <> teams_key = team.teams_key
    token = Livebook.Stamping.aead_encrypt(metadata, notebook_source, teams_key)

    case Livebook.Teams.org_sign(team, token) do
      {:ok, token_signature} ->
        stamp = %{"version" => 1, "token" => token, "token_signature" => token_signature}
        {:ok, stamp}

      _ ->
        {:error, "request to Livebook Teams failed"}
    end
  end

  def verify_notebook_stamp(team, notebook_source, stamp) do
    %{"version" => 1, "token" => token, "token_signature" => token_signature} = stamp

    @teams_key_prefix <> teams_key = team.teams_key
    @public_key_prefix <> org_public_key = team.org_public_key

    if Livebook.Stamping.rsa_verify?(token_signature, token, org_public_key) do
      Livebook.Stamping.aead_decrypt(token, notebook_source, teams_key)
    else
      :error
    end
  end

  def dump(team) do
    team
    |> Map.from_struct()
    |> Map.delete(:offline)
  end
end
