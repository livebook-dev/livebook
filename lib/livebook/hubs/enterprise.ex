defmodule Livebook.Hubs.Enterprise do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Hubs

  @type t :: %__MODULE__{
          id: String.t() | nil,
          url: String.t() | nil,
          token: String.t() | nil,
          external_id: String.t() | nil,
          hub_name: String.t() | nil,
          hub_emoji: String.t() | nil
        }

  embedded_schema do
    field :url, :string
    field :token, :string
    field :external_id, :string
    field :hub_name, :string
    field :hub_emoji, :string
  end

  @fields ~w(
    url
    token
    external_id
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
       |> add_error(:external_id, "already exists")
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
       |> add_error(:external_id, "does not exists")
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
    case get_field(changeset, :external_id) do
      nil -> changeset
      external_id -> put_change(changeset, :id, "enterprise-#{external_id}")
    end
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Enterprise do
  alias Livebook.Hubs.EnterpriseClient

  def load(enterprise, fields) do
    %{
      enterprise
      | id: fields.id,
        url: fields.url,
        token: fields.token,
        external_id: fields.external_id,
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
