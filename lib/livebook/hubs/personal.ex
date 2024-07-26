defmodule Livebook.Hubs.Personal do
  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.FileSystem
  alias Livebook.FileSystems
  alias Livebook.Hubs
  alias Livebook.Storage
  alias Livebook.Secrets.Secret

  @secrets_namespace :hub_secrets
  @secret_key_size 64

  @file_systems_namespace :file_systems

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

  With success, notifies interested processes about hub metadata data change.
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
  Get the secrets list from storage.
  """
  @spec get_secrets() :: list(Secret.t())
  def get_secrets() do
    Enum.map(Storage.all(@secrets_namespace), &to_secret/1)
  end

  @doc """
  Gets a secret from storage.

  Raises `RuntimeError` if the secret doesn't exist.
  """
  @spec fetch_secret!(String.t()) :: Secret.t()
  def fetch_secret!(id) do
    Storage.fetch!(@secrets_namespace, id) |> to_secret()
  end

  @doc """
  Stores the given secret as is, without validation.
  """
  @spec set_secret(Secret.t()) :: Secret.t()
  def set_secret(secret) do
    attributes = Map.from_struct(secret)
    :ok = Storage.insert(@secrets_namespace, secret.name, Map.to_list(attributes))
    secret
  end

  @doc """
  Unset secret from given id.
  """
  @spec unset_secret(String.t()) :: :ok
  def unset_secret(id) do
    Storage.delete(@secrets_namespace, id)
    :ok
  end

  defp to_secret(%{name: name, value: value}) do
    %Secret{
      name: name,
      value: value,
      hub_id: Livebook.Hubs.Personal.id()
    }
  end

  @doc """
  Generates a random secret key used for stamping the notebook.
  """
  @spec generate_secret_key() :: String.t()
  def generate_secret_key() do
    Base.url_encode64(:crypto.strong_rand_bytes(@secret_key_size), padding: false)
  end

  @doc """
  Get the file systems list from storage.
  """
  @spec get_file_systems() :: list(FileSystem.t())
  def get_file_systems() do
    Storage.all(@file_systems_namespace)
    |> Enum.sort_by(& &1.bucket_url)
    |> Enum.map(&to_file_system/1)
  end

  @doc """
  Gets a file system from storage.

  Raises `RuntimeError` if the file system does not exist.
  """
  @spec fetch_file_system!(String.t()) :: FileSystem.t()
  def fetch_file_system!(id) do
    Storage.fetch!(@file_systems_namespace, id) |> to_file_system()
  end

  @doc """
  Saves a new file system to the configured ones.
  """
  @spec save_file_system(FileSystem.t()) :: FileSystem.t()
  def save_file_system(file_system) do
    attributes = FileSystem.dump(file_system)
    type = FileSystems.type(file_system)
    storage_attributes = Map.put(attributes, :type, type)

    :ok = Storage.insert(@file_systems_namespace, file_system.id, Map.to_list(storage_attributes))

    file_system
  end

  @doc """
  Removes the given file system from the configured ones.
  """
  @spec remove_file_system(FileSystem.id()) :: :ok
  def remove_file_system(id) do
    Storage.delete(@file_systems_namespace, id)
  end

  defp to_file_system(fields) do
    FileSystems.load(fields.type, fields)
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Personal do
  alias Livebook.Hubs.Broadcasts
  alias Livebook.Hubs.Personal

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

  def get_secrets(_personal) do
    Personal.get_secrets()
  end

  def create_secret(_personal, secret) do
    Personal.set_secret(secret)
    :ok = Broadcasts.secret_created(secret)
  end

  def update_secret(_personal, secret) do
    Personal.set_secret(secret)
    :ok = Broadcasts.secret_updated(secret)
  end

  def delete_secret(_personal, secret) do
    :ok = Personal.unset_secret(secret.name)
    :ok = Broadcasts.secret_deleted(secret)
  end

  def connection_status(_personal), do: raise("not implemented")

  def notebook_stamp(_hub, _notebook_source, metadata) when metadata == %{} do
    :skip
  end

  def notebook_stamp(personal, notebook_source, metadata) do
    token = Livebook.Stamping.chapoly_encrypt(metadata, notebook_source, personal.secret_key)
    stamp = %{"version" => 2, "token" => token}
    {:ok, stamp}
  end

  def verify_notebook_stamp(personal, notebook_source, stamp) do
    case stamp do
      %{"version" => 1, "token" => token} ->
        Livebook.Stamping.aead_decrypt(token, notebook_source, personal.secret_key)

      %{"version" => 2, "token" => token} ->
        Livebook.Stamping.chapoly_decrypt(token, notebook_source, personal.secret_key)

      %{"version" => _} ->
        {:error, :too_recent_version}
    end
  end

  def dump(personal) do
    Map.from_struct(personal)
  end

  def get_file_systems(_personal) do
    Personal.get_file_systems()
  end

  def create_file_system(_personal, file_system) do
    Personal.save_file_system(file_system)
    :ok = Broadcasts.file_system_created(file_system)
  end

  def update_file_system(_personal, file_system) do
    Personal.save_file_system(file_system)
    :ok = Broadcasts.file_system_updated(file_system)
  end

  def delete_file_system(_personal, file_system) do
    :ok = Personal.remove_file_system(file_system.id)
    :ok = Broadcasts.file_system_deleted(file_system)
  end

  def deployment_groups(_personal), do: nil

  def get_app_specs(_personal), do: []
end
