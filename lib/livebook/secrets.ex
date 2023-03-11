defmodule Livebook.Secrets do
  @moduledoc false

  alias Livebook.Hubs.Provider
  alias Livebook.Storage
  alias Livebook.Secrets.Secret

  @namespace :hub_secrets

  @doc """
  Get the secrets list from storage.
  """
  @spec get_secrets(Provider.t()) :: list(Secret.t())
  def get_secrets(hub) do
    for fields <- Storage.all(@namespace),
        from_hub?(fields, hub),
        do: to_struct(fields)
  end

  @doc """
  Gets a secret from storage.
  Raises `RuntimeError` if the secret doesn't exist.
  """
  @spec fetch_secret!(Provider.t(), String.t()) :: Secret.t()
  def fetch_secret!(hub, id) do
    fields = Storage.fetch!(@namespace, id)
    true = from_hub?(fields, hub)

    to_struct(fields)
  end

  @doc """
  Gets a secret from storage.
  """
  @spec get_secret(Provider.t(), String.t()) :: {:ok, Secret.t()} | :error
  def get_secret(hub, id) do
    with {:ok, fields} <- Storage.fetch(@namespace, id) do
      if from_hub?(fields, hub),
        do: {:ok, to_struct(fields)},
        else: :error
    end
  end

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

  @doc """
  Returns an `%Ecto.Changeset{}` with errors.
  """
  @spec add_secret_error(Ecto.Changeset.t() | Secret.t(), atom(), String.t()) ::
          Ecto.Changeset.t()
  def add_secret_error(%Secret{} = secret, field, message) do
    secret
    |> change_secret(%{})
    |> Ecto.Changeset.add_error(field, message)
  end

  def add_secret_error(%Ecto.Changeset{} = changeset, field, message) do
    Ecto.Changeset.add_error(changeset, field, message)
  end

  @doc """
  Stores the given secret as is, without validation.
  """
  @spec set_secret(Secret.t()) :: Secret.t()
  def set_secret(secret) do
    attributes =
      secret
      |> Map.from_struct()
      |> Map.delete(:readonly)

    :ok = Storage.insert(@namespace, secret.name, Map.to_list(attributes))

    secret
  end

  @doc """
  Unset secret from given id.
  """
  @spec unset_secret(Provider.t(), String.t()) :: :ok
  def unset_secret(hub, id) do
    with {:ok, _secret} <- get_secret(hub, id) do
      Storage.delete(@namespace, id)
    end

    :ok
  end

  defp to_struct(%{name: name, value: value} = fields) do
    %Secret{
      name: name,
      value: value,
      hub_id: fields[:hub_id] || Livebook.Hubs.Personal.id(),
      readonly: false
    }
  end

  defp from_hub?(fields, hub) do
    hub_id = fields[:hub_id] || Livebook.Hubs.Personal.id()
    hub_id == hub.id
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
