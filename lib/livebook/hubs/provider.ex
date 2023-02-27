defprotocol Livebook.Hubs.Provider do
  @moduledoc false

  alias Livebook.Secrets.Secret

  @type t :: Livebook.Hubs.Enterprise.t() | Livebook.Hubs.Fly.t() | Livebook.Hubs.Personal.t()
  @type capability ::
          :connect | :secrets | :list_secrets | :create_secret | :update_secret | :delete_secret
  @type capabilities :: list(capability())
  @type changeset_errors :: %{required(:errors) => list({String.t(), {Stirng.t(), list()}})}

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
  Gets the capabilities of the given hub.
  """
  @spec capabilities(t()) :: capabilities()
  def capabilities(hub)

  @doc """
  Gets the secrets of the given hub.
  """
  @spec get_secrets(t()) :: list(Secret.t())
  def get_secrets(hub)

  @doc """
  Creates a secret of the given hub.
  """
  @spec create_secret(t(), Secret.t()) :: :ok | {:error, changeset_errors()}
  def create_secret(hub, secret)

  @doc """
  Updates a secret of the given hub.
  """
  @spec update_secret(t(), Secret.t()) :: :ok | {:error, changeset_errors()}
  def update_secret(hub, secret)

  @doc """
  Deletes a secret of the given hub.
  """
  @spec delete_secret(t(), Secret.t()) :: :ok | {:error, changeset_errors()}
  def delete_secret(hub, secret)

  @doc """
  Gets the connection error from hub.
  """
  @spec connection_error(t()) :: String.t() | nil
  def connection_error(hub)
end
