defmodule Livebook.Secrets do
  @moduledoc false

  import Ecto.Changeset, only: [apply_action: 2]

  alias Livebook.Secrets.Secret

  defmodule NotFoundError do
    @moduledoc false

    defexception [:message, plug_status: 404]
  end

  defp storage() do
    Livebook.Storage.current()
  end

  @doc """
  Get the secrets list from storage.
  """
  @spec fetch_secrets() :: list(Secret.t())
  def fetch_secrets() do
    for fields <- storage().all(:secrets) do
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
    case storage().fetch(:secrets, id) do
      :error ->
        raise NotFoundError, "could not find the secret matching #{inspect(id)}"

      {:ok, fields} ->
        struct!(Secret, Map.delete(fields, :id))
    end
  end

  @doc """
  Checks if the secret already exists.
  """
  @spec secret_exists?(String.t()) :: boolean()
  def secret_exists?(id) do
    storage().fetch(:secrets, id) != :error
  end

  @doc """
  Sets the given secret.
  """
  @spec set_secret(Secret.t() | %Secret{}, map()) ::
          {:ok, Secret.t()} | {:error, Ecto.Changeset.t()}
  def set_secret(%Secret{} = secret \\ %Secret{}, attrs) do
    changeset = Secret.changeset(secret, attrs)

    with {:ok, secret} <- apply_action(changeset, :insert) do
      save_secret(secret)
    end
  end

  defp save_secret(secret) do
    attributes = secret |> Map.from_struct() |> Map.to_list()

    with :ok <- storage().insert(:secrets, secret.name, attributes),
         :ok <- broadcast_secrets_change({:set_secret, secret}) do
      {:ok, secret}
    end
  end

  @doc """
  Unset secret from given id.
  """
  @spec unset_secret(String.t()) :: :ok
  def unset_secret(id) do
    if secret_exists?(id) do
      secret = fetch_secret!(id)
      storage().delete(:secrets, id)
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
