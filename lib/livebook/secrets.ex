defmodule Livebook.Secrets do
  @moduledoc false

  alias Livebook.Hubs.Provider
  alias Livebook.Storage
  alias Livebook.Secrets.Secret

  @doc """
  Get the secrets list from storage.
  """
  @spec get_secrets(Provider.t()) :: list(Secret.t())
  def get_secrets(hub) do
    for fields <- Storage.all(hub.id),
        do: to_struct(fields)
  end

  @doc """
  Gets a secret from storage.
  Raises `RuntimeError` if the secret doesn't exist.
  """
  @spec fetch_secret!(Provider.t(), String.t()) :: Secret.t()
  def fetch_secret!(hub, id) do
    hub.id
    |> Storage.fetch!(id)
    |> to_struct()
  end

  @doc """
  Gets a secret from storage.
  """
  @spec get_secret(Provider.t(), String.t()) :: {:ok, Secret.t()} | :error
  def get_secret(hub, id) do
    with {:ok, fields} <- Storage.fetch(hub.id, id) do
      {:ok, to_struct(fields)}
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
  @spec set_secret(Provider.t(), Secret.t()) :: Secret.t()
  def set_secret(hub, secret) do
    attributes =
      secret
      |> Map.from_struct()
      |> Map.delete(:readonly)

    :ok = Storage.insert(hub.id, secret.name, Map.to_list(attributes))

    secret
  end

  @doc """
  Unset secret from given id.
  """
  @spec unset_secret(Provider.t(), String.t()) :: :ok
  def unset_secret(hub, id) do
    with {:ok, _secret} <- get_secret(hub, id) do
      Storage.delete(hub.id, id)
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
end
