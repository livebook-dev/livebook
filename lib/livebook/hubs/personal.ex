defmodule Livebook.Hubs.Personal do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Hubs

  @secret_key_size 64

  @type t :: %__MODULE__{
          id: String.t() | nil,
          hub_name: String.t() | nil,
          hub_emoji: String.t() | nil,
          secret_key: String.t() | nil
        }

  embedded_schema do
    field :hub_name, :string
    field :hub_emoji, :string
    field :secret_key, :string, redact: true
  end

  @fields ~w(hub_name hub_emoji secret_key)a

  @doc """
  The personal hub fixed id.
  """
  @spec id() :: String.t()
  def id, do: "personal-hub"

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking hub changes.
  """
  @spec change_hub(t(), map()) :: Ecto.Changeset.t()
  def change_hub(%__MODULE__{} = personal, attrs \\ %{}) do
    changeset(personal, attrs)
  end

  @doc """
  Returns changeset with applied validations.
  """
  @spec validate_hub(t(), map()) :: Ecto.Changeset.t()
  def validate_hub(%__MODULE__{} = personal, attrs \\ %{}) do
    personal
    |> changeset(attrs)
    |> Map.put(:action, :validate)
  end

  @doc """
  Updates a Hub.

  With success, notifies interested processes about hub metadatas data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec update_hub(t(), map()) :: {:ok, t()} | {:error, Ecto.Changeset.t()}
  def update_hub(%__MODULE__{} = personal, attrs) do
    changeset = changeset(personal, attrs)

    with {:ok, struct} <- apply_action(changeset, :update) do
      Hubs.save_hub(struct)
      {:ok, struct}
    end
  end

  defp changeset(personal, attrs) do
    personal
    |> cast(attrs, @fields)
    |> validate_required(@fields)
    |> validate_change(:secret_key, fn :secret_key, secret_key ->
      case Base.url_decode64(secret_key, padding: false) do
        {:ok, binary} when byte_size(binary) == @secret_key_size -> []
        _ -> [secret_key: "must be #{@secret_key_size} bytes in Base 64 URL alphabet"]
      end
    end)
    |> put_change(:id, id())
  end

  @doc """
  Generates a random secret key used for stamping the notebook.
  """
  @spec generate_secret_key() :: String.t()
  def generate_secret_key() do
    :crypto.strong_rand_bytes(@secret_key_size) |> Base.url_encode64(padding: false)
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Personal do
  alias Livebook.Hubs.Broadcasts
  alias Livebook.Secrets

  def load(personal, fields) do
    %{
      personal
      | id: fields.id,
        hub_name: fields.hub_name,
        hub_emoji: fields.hub_emoji,
        secret_key: fields.secret_key
    }
  end

  def to_metadata(personal) do
    %Livebook.Hubs.Metadata{
      id: personal.id,
      name: personal.hub_name,
      provider: personal,
      emoji: personal.hub_emoji,
      connected?: false
    }
  end

  def type(_personal), do: "personal"

  def connection_spec(_personal), do: nil

  def disconnect(_personal), do: raise("not implemented")

  def capabilities(_personal), do: ~w(list_secrets create_secret)a

  def get_secrets(personal) do
    Secrets.get_secrets(personal)
  end

  def create_secret(_personal, secret) do
    Secrets.set_secret(secret)
    :ok = Broadcasts.secret_created(secret)
  end

  def update_secret(_personal, secret) do
    Secrets.set_secret(secret)
    :ok = Broadcasts.secret_updated(secret)
  end

  def delete_secret(personal, secret) do
    :ok = Secrets.unset_secret(personal, secret.name)
    :ok = Broadcasts.secret_deleted(secret)
  end

  def connection_error(_personal), do: raise("not implemented")

  def notebook_stamp(_hub, _notebook_source, metadata) when metadata == %{} do
    :skip
  end

  def notebook_stamp(hub, notebook_source, metadata) do
    # We use AES-GCM-128 to encrypt metadata and generate signature
    # for both metadata and the notebook source. We make use of the
    # implementation in MessageEncryptor, which conveniently returns
    # a single token

    {secret, sign_secret} = derive_keys(hub.secret_key)

    payload = :erlang.term_to_binary(metadata)
    token = Plug.Crypto.MessageEncryptor.encrypt(payload, notebook_source, secret, sign_secret)

    stamp = %{"version" => 1, "token" => token}

    {:ok, stamp}
  end

  def verify_notebook_stamp(hub, notebook_source, stamp) do
    %{"version" => 1, "token" => token} = stamp

    {secret, sign_secret} = derive_keys(hub.secret_key)

    case Plug.Crypto.MessageEncryptor.decrypt(token, notebook_source, secret, sign_secret) do
      {:ok, payload} ->
        metadata = Plug.Crypto.non_executable_binary_to_term(payload)
        {:ok, metadata}

      :error ->
        :error
    end
  end

  defp derive_keys(secret_key) do
    binary_key = Base.url_decode64!(secret_key, padding: false)

    <<secret::16-bytes, sign_secret::16-bytes>> =
      Plug.Crypto.KeyGenerator.generate(binary_key, "notebook signing",
        length: 32,
        cache: Plug.Crypto.Keys
      )

    {secret, sign_secret}
  end
end
