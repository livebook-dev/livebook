defmodule Livebook.Hubs.Broadcasts do
  @moduledoc false

  alias Livebook.Secrets.Secret

  @type broadcast :: :ok | {:error, term()}

  @crud_topic "hubs:crud"
  @connection_topic "hubs:connection"
  @secrets_topic "hubs:secrets"

  @doc """
  Broadcasts when hubs changed under `hubs:crud` topic
  """
  @spec hubs_metadata_changed() :: broadcast()
  def hubs_metadata_changed do
    broadcast(@crud_topic, :hubs_metadata_changed)
  end

  @doc """
  Broadcasts when hub connected under `hubs:connection` topic
  """
  @spec hub_connected() :: broadcast()
  def hub_connected do
    broadcast(@connection_topic, :hub_connected)
  end

  @doc """
  Broadcasts when hub disconnected under `hubs:connection` topic
  """
  @spec hub_disconnected() :: broadcast()
  def hub_disconnected do
    broadcast(@connection_topic, :hub_disconnected)
  end

  @doc """
  Broadcasts when hub had an error when connecting under `hubs:connection` topic
  """
  @spec hub_connection_failed(String.t()) :: broadcast()
  def hub_connection_failed(reason) when is_binary(reason) do
    broadcast(@connection_topic, {:hub_connection_failed, reason})
  end

  @doc """
  Broadcasts when hub had an error when disconnecting under `hubs:connection` topic
  """
  @spec hub_disconnection_failed(String.t()) :: broadcast()
  def hub_disconnection_failed(reason) when is_binary(reason) do
    broadcast(@connection_topic, {:hub_disconnection_failed, reason})
  end

  @doc """
  Broadcasts when hub received a new secret under `hubs:secrets` topic
  """
  @spec secret_created(Secret.t()) :: broadcast()
  def secret_created(%Secret{} = secret) do
    broadcast(@secrets_topic, {:secret_created, secret})
  end

  @doc """
  Broadcasts when hub received an updated secret under `hubs:secrets` topic
  """
  @spec secret_updated(Secret.t()) :: broadcast()
  def secret_updated(%Secret{} = secret) do
    broadcast(@secrets_topic, {:secret_updated, secret})
  end

  defp broadcast(topic, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, topic, message)
  end
end
