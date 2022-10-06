defmodule Livebook.Secrets do
  @moduledoc false

  import Ecto.Changeset, only: [apply_action: 2]

  alias Livebook.Storage
  alias Livebook.Secrets.Secret

  @doc """
  Get the secrets list from storage.
  """
  @spec fetch_secrets() :: list(Secret.t())
  def fetch_secrets() do
    for fields <- Storage.all(:secrets) do
      struct!(Secret, Map.delete(fields, :id))
    end
    |> Enum.sort()
  end

  @doc """
  Gets a secret from storage.
  Raises `RuntimeError` if the secret doesn't exist.
  """
  @spec fetch_secret!(String.t()) :: Secret.t()
  def fetch_secret!(id) do
    fields = Storage.fetch!(:secrets, id)
    struct!(Secret, Map.delete(fields, :id))
  end

  @doc """
  Checks if the secret already exists.
  """
  @spec secret_exists?(String.t()) :: boolean()
  def secret_exists?(id) do
    Storage.fetch(:secrets, id) != :error
  end

  @doc """
  Validates a secret map and either returns a struct struct or changeset.
  """
  @spec validate_secret(map()) :: {:ok, Secret.t()} | {:error, Ecto.Changeset.t()}
  def validate_secret(attrs) do
    changeset = Secret.changeset(%Secret{}, attrs)
    apply_action(changeset, :validate)
  end

  @doc """
  Stores the given secret as is, without validation.
  """
  @spec set_secret(Secret.t()) :: Secret.t()
  def set_secret(secret) do
    attributes = secret |> Map.from_struct() |> Map.to_list()
    :ok = Storage.insert(:secrets, secret.name, attributes)
    :ok = broadcast_secrets_change({:set_secret, secret})
    secret
  end

  @doc """
  Unset secret from given id.
  """
  @spec unset_secret(String.t()) :: :ok
  def unset_secret(id) do
    if secret_exists?(id) do
      secret = fetch_secret!(id)
      Storage.delete(:secrets, id)
      broadcast_secrets_change({:unset_secret, secret})
    end

    :ok
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
end
