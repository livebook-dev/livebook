defmodule Livebook.Hubs.Personal do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Hubs

  @type t :: %__MODULE__{
          id: String.t() | nil,
          hub_name: String.t() | nil,
          hub_emoji: String.t() | nil
        }

  embedded_schema do
    field :hub_name, :string
    field :hub_emoji, :string
  end

  @fields ~w(hub_name hub_emoji)a

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
    |> put_change(:id, id())
  end

  @secret_startup_key :livebook_startup_secrets

  @doc """
  Get the startup secrets list from persistent term.
  """
  @spec get_startup_secrets() :: list(Secret.t())
  def get_startup_secrets do
    :persistent_term.get(@secret_startup_key, [])
  end

  @doc """
  Sets additional secrets that are kept only in memory.
  """
  @spec set_startup_secrets(list(Secret.t())) :: :ok
  def set_startup_secrets(secrets) do
    :persistent_term.put(@secret_startup_key, secrets)
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Personal do
  alias Livebook.Hubs.Broadcasts
  alias Livebook.Secrets

  def load(personal, fields) do
    %{personal | id: fields.id, hub_name: fields.hub_name, hub_emoji: fields.hub_emoji}
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
    Secrets.get_secrets(personal) ++ Livebook.Hubs.Personal.get_startup_secrets()
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
end
