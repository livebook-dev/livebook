defmodule Livebook.Hubs.Broadcasts do
  @moduledoc false

  alias Livebook.Secrets.Secret

  @type broadcast :: :ok | {:error, term()}

  @crud_topic "hubs:crud"
  @connection_topic "hubs:connection"
  @secrets_topic "hubs:secrets"

  @doc """
  Broadcasts under `hubs:crud` topic when hubs changed.
  """
  @spec hub_changed() :: broadcast()
  def hub_changed do
    broadcast(@crud_topic, :hub_changed)
  end

  @doc """
  Broadcasts under `hubs:connection` topic when hub connected.
  """
  @spec hub_connected() :: broadcast()
  def hub_connected do
    broadcast(@connection_topic, :hub_connected)
  end

  @doc """
  Broadcasts under `hubs:connection` topic when hub disconnected.
  """
  @spec hub_disconnected() :: broadcast()
  def hub_disconnected do
    broadcast(@connection_topic, :hub_disconnected)
  end

  @doc """
  Broadcasts under `hubs:connection` topic when hub received a connection error.
  """
  @spec hub_connection_failed(String.t()) :: broadcast()
  def hub_connection_failed(reason) when is_binary(reason) do
    broadcast(@connection_topic, {:hub_connection_failed, reason})
  end

  @doc """
  Broadcasts under `hubs:secrets` topic when hub received a new secret.
  """
  @spec secret_created(Secret.t()) :: broadcast()
  def secret_created(%Secret{} = secret) do
    broadcast(@secrets_topic, {:secret_created, secret})
  end

  @doc """
  Broadcasts under `hubs:secrets` topic when hub received an updated secret.
  """
  @spec secret_updated(Secret.t()) :: broadcast()
  def secret_updated(%Secret{} = secret) do
    broadcast(@secrets_topic, {:secret_updated, secret})
  end

  @doc """
  Broadcasts under `hubs:secrets` topic when hub received a deleted secret.
  """
  @spec secret_deleted(Secret.t()) :: broadcast()
  def secret_deleted(%Secret{} = secret) do
    broadcast(@secrets_topic, {:secret_deleted, secret})
  end

  defp broadcast(topic, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, topic, message)
  end
end
