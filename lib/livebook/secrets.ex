defmodule Livebook.Secrets do
  # Shared secret functionality across all hubs.
  @moduledoc false

  alias Livebook.Secrets.Secret

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking secret changes.
  """
  @spec change_secret(Secret.t(), map()) :: Ecto.Changeset.t()
  def change_secret(%Secret{} = secret, attrs) do
    Secret.changeset(secret, attrs)
  end

  @doc """
  Updates secret with the given changes.
  """
  @spec update_secret(Secret.t(), map()) :: {:ok, Secret.t()} | {:error, Ecto.Changeset.t()}
  def update_secret(%Secret{} = secret, attrs) do
    secret
    |> Secret.changeset(attrs)
    |> Ecto.Changeset.apply_action(:update)
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
