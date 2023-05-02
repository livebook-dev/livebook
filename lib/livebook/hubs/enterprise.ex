defmodule Livebook.Hubs.Enterprise do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Hubs

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
  def change_hub(%__MODULE__{} = enterprise, attrs \\ %{}) do
    changeset(enterprise, attrs)
  end

  @doc """
  Returns changeset with applied validations.
  """
  @spec validate_hub(t(), map()) :: Ecto.Changeset.t()
  def validate_hub(%__MODULE__{} = enterprise, attrs \\ %{}) do
    enterprise
    |> changeset(attrs)
    |> Map.put(:action, :validate)
  end

  @doc """
  Creates a Hub.

  With success, notifies interested processes about hub metadatas data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec create_hub(t(), map()) :: {:ok, t()} | {:error, Ecto.Changeset.t()}
  def create_hub(%__MODULE__{} = enterprise, attrs) do
    changeset = changeset(enterprise, attrs)
    id = get_field(changeset, :id)

    if Hubs.hub_exists?(id) do
      {:error,
       changeset
       |> add_error(:hub_name, "already exists")
       |> Map.replace!(:action, :validate)}
    else
      with {:ok, struct} <- apply_action(changeset, :insert) do
        Hubs.save_hub(struct)
        {:ok, struct}
      end
    end
  end

  @doc """
  Updates a Hub.

  With success, notifies interested processes about hub metadatas data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec update_hub(t(), map()) :: {:ok, t()} | {:error, Ecto.Changeset.t()}
  def update_hub(%__MODULE__{} = enterprise, attrs) do
    changeset = changeset(enterprise, attrs)
    id = get_field(changeset, :id)

    if Hubs.hub_exists?(id) do
      with {:ok, struct} <- apply_action(changeset, :update) do
        Hubs.save_hub(struct)
        {:ok, struct}
      end
    else
      {:error,
       changeset
       |> add_error(:hub_name, "does not exists")
       |> Map.replace!(:action, :validate)}
    end
  end

  defp changeset(enterprise, attrs) do
    enterprise
    |> cast(attrs, @fields)
    |> validate_required(@fields)
    |> add_id()
  end

  defp add_id(changeset) do
    case get_field(changeset, :hub_name) do
      nil -> changeset
      hub_name -> put_change(changeset, :id, "enterprise-#{hub_name}")
    end
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Enterprise do
  alias Livebook.Hubs.EnterpriseClient

  def load(enterprise, fields) do
    %{
      enterprise
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

  def to_metadata(enterprise) do
    %Livebook.Hubs.Metadata{
      id: enterprise.id,
      name: enterprise.hub_name,
      provider: enterprise,
      emoji: enterprise.hub_emoji,
      connected?: EnterpriseClient.connected?(enterprise.id)
    }
  end

  def type(_enterprise), do: "enterprise"

  def connection_spec(enterprise), do: {EnterpriseClient, enterprise}

  def disconnect(enterprise) do
    EnterpriseClient.stop(enterprise.id)
  end

  def capabilities(_enterprise), do: ~w(connect list_secrets create_secret)a

  def get_secrets(enterprise) do
    EnterpriseClient.get_secrets(enterprise.id)
  end

  def create_secret(enterprise, secret) do
    data = LivebookProto.build_create_secret_request(name: secret.name, value: secret.value)

    case EnterpriseClient.send_request(enterprise.id, data) do
      {:create_secret, _} ->
        :ok

      {:changeset_error, errors} ->
        changeset =
          for {field, messages} <- errors,
              message <- messages,
              reduce: secret do
            acc ->
              Livebook.Secrets.add_secret_error(acc, field, message)
          end

        {:error, changeset}

      {:transport_error, reason} ->
        message = "#{enterprise.hub_emoji} #{enterprise.hub_name}: #{reason}"
        changeset = Livebook.Secrets.add_secret_error(secret, :hub_id, message)

        {:error, changeset}
    end
  end

  def update_secret(_enterprise, _secret), do: raise("not implemented")

  def delete_secret(_enterprise, _secret), do: raise("not implemented")

  def connection_error(enterprise) do
    reason = EnterpriseClient.get_connection_error(enterprise.id)
    "Cannot connect to Hub: #{reason}. Will attempt to reconnect automatically..."
  end

  # TODO: implement signing through the enterprise server
  def notebook_stamp(_hub, _notebook_source, _metadata) do
    :skip
  end

  def verify_notebook_stamp(_hub, _notebook_source, _stamp), do: raise("not implemented")
end
