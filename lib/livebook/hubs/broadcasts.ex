defmodule Livebook.Hubs.Broadcasts do
  alias Livebook.FileSystem
  alias Livebook.Secrets.Secret

  @type broadcast :: :ok | {:error, term()}

  @crud_topic "hubs:crud"
  @connection_topic "hubs:connection"
  @secrets_topic "hubs:secrets"
  @file_systems_topic "hubs:file_systems"

  @doc """
  Subscribes to one or more subtopics in `"hubs"`.

  ## Messages

  Topic `hubs:crud`:

    * `{:hub_changed, hub_id}`
    * `{:hub_deleted, hub_id}`

  Topic `hubs:connection`:

    * `{:hub_connected, hub_id}`
    * `{:hub_connection_failed, hub_id, reason}`
    * `{:hub_server_error, hub_id, reason}`

  Topic `hubs:secrets`:

    * `{:secret_created, %Secret{}}`
    * `{:secret_updated, %Secret{}}`
    * `{:secret_deleted, %Secret{}}`

  Topic `hubs:file_systems`:

    * `{:file_system_created, FileSystem.t()}`
    * `{:file_system_updated, FileSystem.t()}`
    * `{:file_system_deleted, FileSystem.t()}`

  """
  @spec subscribe(atom() | list(atom())) :: :ok | {:error, term()}
  def subscribe(topics) when is_list(topics) do
    for topic <- topics, do: subscribe(topic)

    :ok
  end

  def subscribe(topic) do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "hubs:#{topic}")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe(atom() | list(atom())) :: :ok
  def unsubscribe(topics) when is_list(topics) do
    for topic <- topics, do: unsubscribe(topic)

    :ok
  end

  def unsubscribe(topic) do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "hubs:#{topic}")
  end

  @doc """
  Broadcasts under `#{@crud_topic}` topic when hubs changed.
  """
  @spec hub_changed(String.t()) :: broadcast()
  def hub_changed(hub_id) do
    broadcast(@crud_topic, {:hub_changed, hub_id})
  end

  @doc """
  Broadcasts under `#{@crud_topic}` topic when hub is deleted.
  """
  @spec hub_deleted(String.t()) :: broadcast()
  def hub_deleted(hub_id) do
    broadcast(@crud_topic, {:hub_deleted, hub_id})
  end

  @doc """
  Broadcasts under `#{@connection_topic}` topic when hub connected.
  """
  @spec hub_connected(String.t()) :: broadcast()
  def hub_connected(hub_id) do
    broadcast(@connection_topic, {:hub_connected, hub_id})
  end

  @doc """
  Broadcasts under `#{@connection_topic}` topic when hub is out-of-date.
  """
  @spec hub_server_error(String.t(), String.t()) :: broadcast()
  def hub_server_error(hub_id, reason) when is_binary(reason) do
    broadcast(@connection_topic, {:hub_server_error, hub_id, reason})
  end

  @doc """
  Broadcasts under `#{@connection_topic}` topic when hub received a connection error.
  """
  @spec hub_connection_failed(String.t(), String.t()) :: broadcast()
  def hub_connection_failed(hub_id, reason) when is_binary(reason) do
    broadcast(@connection_topic, {:hub_connection_failed, hub_id, reason})
  end

  @doc """
  Broadcasts under `#{@secrets_topic}` topic when hub received a new secret.
  """
  @spec secret_created(Secret.t()) :: broadcast()
  def secret_created(%Secret{} = secret) do
    broadcast(@secrets_topic, {:secret_created, secret})
  end

  @doc """
  Broadcasts under `#{@secrets_topic}` topic when hub received an updated secret.
  """
  @spec secret_updated(Secret.t()) :: broadcast()
  def secret_updated(%Secret{} = secret) do
    broadcast(@secrets_topic, {:secret_updated, secret})
  end

  @doc """
  Broadcasts under `#{@secrets_topic}` topic when hub received a deleted secret.
  """
  @spec secret_deleted(Secret.t()) :: broadcast()
  def secret_deleted(%Secret{} = secret) do
    broadcast(@secrets_topic, {:secret_deleted, secret})
  end

  @allowed_file_systems [FileSystem.S3]

  @doc """
  Broadcasts under `#{@file_systems_topic}` topic when hub received a new file system.
  """
  @spec file_system_created(FileSystem.t()) :: broadcast()
  def file_system_created(%struct{} = file_system) when struct in @allowed_file_systems do
    broadcast(@file_systems_topic, {:file_system_created, file_system})
  end

  @doc """
  Broadcasts under `#{@file_systems_topic}` topic when hub received an updated file system.
  """
  @spec file_system_updated(FileSystem.t()) :: broadcast()
  def file_system_updated(%struct{} = file_system) when struct in @allowed_file_systems do
    broadcast(@file_systems_topic, {:file_system_updated, file_system})
  end

  @doc """
  Broadcasts under `#{@file_systems_topic}` topic when hub received a deleted file system.
  """
  @spec file_system_deleted(FileSystem.t()) :: broadcast()
  def file_system_deleted(%struct{} = file_system) when struct in @allowed_file_systems do
    broadcast(@file_systems_topic, {:file_system_deleted, file_system})
  end

  defp broadcast(topic, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, topic, message)
  end
end
