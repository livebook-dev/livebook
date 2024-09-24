defprotocol Livebook.Hubs.Provider do
  alias Livebook.FileSystem
  alias Livebook.Secrets.Secret

  @typedoc """
  An provider-specific map stored as notebook stamp.

  Notebook stamp is meant to serve two purposes:

    1. Signing notebook source to ensure notebook integrity and
       authenticity.

    2. Storing sensitive notebook metadata in encrypted form, so that
       it cannot be read nor modified outside of Livebook.

  Those can be achieved using arbitrary cryptography mechanics. The
  stamp itself is an opaque map, however it must be JSON-compatible
  and use string keys.
  """
  @type notebook_stamp :: map()

  @type field_errors :: list({atom(), list(String.t())})

  @doc """
  Transforms given hub to `Livebook.Hubs.Metadata` struct.
  """
  @spec to_metadata(t()) :: Livebook.Hubs.Metadata.t()
  def to_metadata(hub)

  @doc """
  Loads fields into given hub.
  """
  @spec load(t(), map() | keyword()) :: struct()
  def load(hub, fields)

  @doc """
  Gets the type from hub.
  """
  @spec type(t()) :: String.t()
  def type(hub)

  @doc """
  Gets the child spec of the given hub.
  """
  @spec connection_spec(t()) :: Supervisor.child_spec() | module() | {module(), any()} | nil
  def connection_spec(hub)

  @doc """
  Disconnects the given hub.
  """
  @spec disconnect(t()) :: :ok
  def disconnect(hub)

  @doc """
  Gets the secrets of the given hub.
  """
  @spec get_secrets(t()) :: list(Secret.t())
  def get_secrets(hub)

  @doc """
  Creates a secret of the given hub.
  """
  @spec create_secret(t(), Secret.t()) ::
          :ok
          | {:error, field_errors()}
          | {:transport_error, String.t()}
  def create_secret(hub, secret)

  @doc """
  Updates a secret of the given hub.
  """
  @spec update_secret(t(), Secret.t()) ::
          :ok
          | {:error, field_errors()}
          | {:transport_error, String.t()}
  def update_secret(hub, secret)

  @doc """
  Deletes a secret of the given hub.
  """
  @spec delete_secret(t(), Secret.t()) :: :ok | {:transport_error, String.t()}
  def delete_secret(hub, secret)

  @doc """
  Gets the connection status from hub.
  """
  @spec connection_status(t()) :: String.t() | nil
  def connection_status(hub)

  @doc """
  Generates a notebook stamp.

  See `t:notebook_stamp/0` for more details.
  """
  @spec notebook_stamp(t(), iodata(), map()) ::
          {:ok, notebook_stamp()} | :skip | {:error, String.t()}
  def notebook_stamp(hub, notebook_source, metadata)

  @doc """
  Verifies a notebook stamp and returns the decrypted metadata.

  See `t:notebook_stamp/0` for more details.
  """
  @spec verify_notebook_stamp(t(), iodata(), notebook_stamp()) ::
          {:ok, metadata :: map()} | {:error, :invalid | :too_recent_version}
  def verify_notebook_stamp(hub, notebook_source, stamp)

  @doc """
  Transforms hub to the attributes map sent to storage.
  """
  @spec dump(t()) :: map()
  def dump(hub)

  @doc """
  Gets the file systems of given hub.
  """
  @spec get_file_systems(t()) :: list(FileSystem.t())
  def get_file_systems(hub)

  @doc """
  Creates a file system of the given hub.
  """
  @spec create_file_system(t(), FileSystem.t()) ::
          :ok
          | {:error, field_errors()}
          | {:transport_error, String.t()}
  def create_file_system(hub, file_system)

  @doc """
  Updates a file system of the given hub.
  """
  @spec update_file_system(t(), FileSystem.t()) ::
          :ok
          | {:error, field_errors()}
          | {:transport_error, String.t()}
  def update_file_system(hub, file_system)

  @doc """
  Deletes a file system of the given hub.
  """
  @spec delete_file_system(t(), FileSystem.t()) :: :ok | {:transport_error, String.t()}
  def delete_file_system(hub, file_system)

  @doc """
  Get the deployment groups for a given hub.

  Returns `nil` if deployment groups are not applicable to this hub.
  """
  @spec deployment_groups(t()) ::
          list(%{id: String.t(), name: String.t(), secrets: list(Secret.t())}) | nil
  def deployment_groups(hub)

  @doc """
  Gets app specs for permanent apps sourced from the given hub.
  """
  @spec get_app_specs(t()) :: list(Livebook.Apps.AppSpec.t())
  def get_app_specs(hub)
end
