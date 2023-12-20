defmodule Livebook.Hubs.Team do
  use Ecto.Schema
  import Ecto.Changeset

  defmodule Offline do
    use Ecto.Schema

    @type t :: %__MODULE__{
            file_systems: list(Livebook.FileSystem.t()),
            secrets: list(Livebook.Secrets.Secret.t())
          }

    @primary_key false
    embedded_schema do
      field :secrets, {:array, :map}, default: []
      field :file_systems, {:array, :map}, default: []
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
    field :session_token, :string, redact: true
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

  @editable_fields ~w(hub_emoji)a

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

  def creation_changeset(team, attrs) do
    team
    |> cast(attrs, @fields)
    |> validate_required(@fields)
    |> add_id()
  end

  def update_changeset(team, attrs) do
    team
    |> cast(attrs, @editable_fields)
    |> validate_required(@editable_fields)
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
  alias Livebook.Hubs.{Team, TeamClient}
  alias Livebook.Teams.Requests
  alias Livebook.FileSystem
  alias Livebook.Secrets.Secret

  @teams_key_prefix Livebook.Teams.Org.teams_key_prefix()
  @public_key_prefix Livebook.Hubs.Team.public_key_prefix()

  def load(team, fields) do
    {offline?, fields} = Map.pop(fields, :offline?, false)

    offline =
      # We don't want to persist offline in storage, so we read from persistent term
      if offline? do
        :persistent_term.get({__MODULE__, :offline, fields.id})
      end

    team
    |> struct(fields)
    |> Map.replace!(:offline, offline)
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

  def connection_error(team) do
    cond do
      team.offline ->
        "You are running an offline Hub for deployment. You cannot modify its settings."

      reason = TeamClient.get_connection_error(team.id) ->
        "Cannot connect to Hub: #{reason}.\nWill attempt to reconnect automatically..."

      true ->
        nil
    end
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
    token = Livebook.Stamping.chapoly_encrypt(metadata, notebook_source, teams_key)

    case Requests.org_sign(team, token) do
      {:ok, %{"signature" => token_signature}} ->
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
      Livebook.Stamping.chapoly_decrypt(token, notebook_source, teams_key)
    else
      {:error, :invalid}
    end
  end

  def dump(team) do
    # Offline hub is kept in storage, but only during the lifetime of
    # the runtime (we remove it on the subsequent startup). With this
    # assumption we can safely store the %Offline{} struct in memory,
    # so that the secrets are never written to disk.
    if team.offline do
      :persistent_term.put({__MODULE__, :offline, team.id}, team.offline)
    end

    team
    |> Map.from_struct()
    |> Map.delete(:offline)
    |> Map.put(:offline?, team.offline != nil)
  end

  def get_secrets(team), do: TeamClient.get_secrets(team.id)

  @spec create_secret(Team.t(), Secret.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def create_secret(%Team{} = team, %Secret{} = secret) do
    case Requests.create_secret(team, secret) do
      {:ok, %{"id" => _}} -> :ok
      {:error, %{"errors" => errors}} -> {:error, add_secret_errors(secret, errors)}
      any -> any
    end
  end

  @spec update_secret(Team.t(), Secret.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def update_secret(%Team{} = team, %Secret{} = secret) do
    case Requests.update_secret(team, secret) do
      {:ok, %{"id" => _}} -> :ok
      {:error, %{"errors" => errors}} -> {:error, add_secret_errors(secret, errors)}
      any -> any
    end
  end

  @spec delete_secret(Team.t(), Secret.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def delete_secret(%Team{} = team, %Secret{} = secret) do
    case Requests.delete_secret(team, secret) do
      {:ok, _} -> :ok
      {:error, %{"errors" => errors}} -> {:error, add_secret_errors(secret, errors)}
      any -> any
    end
  end

  def get_file_systems(team), do: TeamClient.get_file_systems(team.id)

  @spec create_file_system(Team.t(), FileSystem.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def create_file_system(%Team{} = team, file_system) do
    case Requests.create_file_system(team, file_system) do
      {:ok, %{"id" => _}} -> :ok
      {:error, %{"errors" => errors}} -> {:error, add_file_system_errors(file_system, errors)}
      any -> any
    end
  end

  @spec update_file_system(Team.t(), FileSystem.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def update_file_system(%Team{} = team, file_system) do
    case Requests.update_file_system(team, file_system) do
      {:ok, %{"id" => _}} -> :ok
      {:error, %{"errors" => errors}} -> {:error, add_file_system_errors(file_system, errors)}
      any -> any
    end
  end

  @spec delete_file_system(Team.t(), FileSystem.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def delete_file_system(%Team{} = team, file_system) do
    case Requests.delete_file_system(team, file_system) do
      {:ok, _} -> :ok
      {:error, %{"errors" => errors}} -> {:error, add_file_system_errors(file_system, errors)}
      any -> any
    end
  end

  defp add_secret_errors(%Secret{} = secret, errors_map) do
    Requests.add_errors(secret, errors_map)
  end

  defp add_file_system_errors(file_system, errors_map) do
    %{error_field: field} = FileSystem.external_metadata(file_system)
    errors_map = Map.new(errors_map, fn {_key, values} -> {field, values} end)
    Requests.add_errors(file_system, errors_map)
  end
end
