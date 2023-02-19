defmodule Livebook.Secrets do
  @moduledoc false

  import Ecto.Changeset, only: [apply_action: 2, add_error: 3, get_field: 2, put_change: 3]

  alias Livebook.Storage
  alias Livebook.Secrets.Secret

  @temporary_key :livebook_temporary_secrets

  @doc """
  Get the secrets list from storage.
  """
  @spec get_secrets() :: list(Secret.t())
  def get_secrets do
    temporary_secrets = :persistent_term.get(@temporary_key, [])

    for fields <- Storage.all(:secrets) do
      to_struct(fields)
    end
    |> Enum.concat(temporary_secrets)
  end

  @doc """
  Gets a secret from storage.
  Raises `RuntimeError` if the secret doesn't exist.
  """
  @spec fetch_secret!(String.t()) :: Secret.t()
  def fetch_secret!(id) do
    fields = Storage.fetch!(:secrets, id)
    to_struct(fields)
  end

  @doc """
  Gets a secret from storage.
  """
  @spec get_secret(String.t()) :: {:ok, Secret.t()} | :error
  def get_secret(id) do
    with {:ok, fields} <- Storage.fetch(:secrets, id) do
      {:ok, to_struct(fields)}
    end
  end

  @doc """
  Checks if the secret already exists.
  """
  @spec secret_exists?(String.t()) :: boolean()
  def secret_exists?(id) do
    Storage.fetch(:secrets, id) != :error
  end

  @doc """
  Validates a secret map and either returns a tuple.

  ## Examples

      iex> validate_secret(%{name: "FOO", value: "bar", origin: "session"})
      {:ok, %Secret{}}

      iex> validate_secret(%{})
      {:error, %Ecto.Changeset{}}

  """
  @spec validate_secret(map()) :: {:ok, Secret.t()} | {:error, Ecto.Changeset.t()}
  def validate_secret(attrs) do
    %Secret{}
    |> Secret.changeset(attrs)
    |> apply_action(:validate)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking secret changes.
  """
  @spec change_secret(Secret.t(), map()) :: Ecto.Changeset.t()
  def change_secret(%Secret{} = secret, attrs) do
    secret
    |> Secret.changeset(attrs)
    |> Map.replace!(:action, :validate)
    |> normalize_origin()
  end

  defp normalize_origin(changeset) do
    case get_field(changeset, :origin) do
      {:hub, id} -> put_change(changeset, :origin, "hub-#{id}")
      _ -> changeset
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` with errors.
  """
  @spec add_secret_error(Ecto.Changeset.t() | Secret.t(), atom(), String.t()) ::
          Ecto.Changeset.t()
  def add_secret_error(%Secret{} = secret, field, message) do
    secret
    |> change_secret(%{})
    |> add_error(field, message)
  end

  def add_secret_error(%Ecto.Changeset{} = changeset, field, message) do
    add_error(changeset, field, message)
  end

  @doc """
  Stores the given secret as is, without validation.
  """
  @spec set_secret(Secret.t()) :: Secret.t()
  def set_secret(secret) do
    attributes = Map.from_struct(secret)

    :ok = Storage.insert(:secrets, secret.name, Map.to_list(attributes))
    :ok = broadcast_secrets_change({:set_secret, secret})

    to_struct(attributes)
  end

  @doc """
  Unset secret from given id.
  """
  @spec unset_secret(String.t()) :: :ok
  def unset_secret(id) do
    with {:ok, secret} <- get_secret(id) do
      Storage.delete(:secrets, id)
      broadcast_secrets_change({:unset_secret, secret})
    end

    :ok
  end

  @doc """
  Sets additional secrets that are kept only in memory.
  """
  @spec set_temporary_secrets(list(Secret.t())) :: :ok
  def set_temporary_secrets(secrets) do
    :persistent_term.put(@temporary_key, secrets)
  end

  @doc """
  Subscribe to secrets updates.

  ## Messages

    * `{:set_secret, secret}`
    * `{:unset_secret, secret}`

  """
  @spec subscribe() :: :ok | {:error, term()}
  def subscribe do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "secrets")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe() :: :ok
  def unsubscribe do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "secrets")
  end

  defp broadcast_secrets_change(message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "secrets", message)
  end

  defp to_struct(%{name: name, value: value} = fields) do
    # Previously stored secrets were all `:app`-based secrets
    %Secret{name: name, value: value, origin: fields[:origin] || :app}
  end
end
