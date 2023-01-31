defprotocol Livebook.Hubs.Provider do
  @moduledoc false

  alias Livebook.Secrets.Secret

  @type capability :: :connect | :secrets
  @type capabilities :: list(capability())
  @type changeset_errors :: list({String.t(), {Stirng.t(), list()}})

  @doc """
  Normalize given hub to `Livebook.Hubs.Metadata` struct.
  """
  @spec normalize(struct()) :: Livebook.Hubs.Metadata.t()
  def normalize(struct)

  @doc """
  Loads fields into given hub.
  """
  @spec load(struct(), map() | keyword()) :: struct()
  def load(struct, fields)

  @doc """
  Gets the type from hub.
  """
  @spec type(struct()) :: String.t()
  def type(struct)

  @doc """
  Gets the child spec of the given hub.
  """
  @spec connect(struct()) :: Supervisor.child_spec() | module() | {module(), any()} | nil
  def connect(struct)

  @doc """
  Gets the connection status of the given hub.
  """
  @spec connected?(struct()) :: boolean()
  def connected?(struct)

  @doc """
  Disconnects the given hub.
  """
  @spec disconnect(struct()) :: :ok
  def disconnect(struct)

  @doc """
  Gets the capabilities of the given hub.
  """
  @spec capabilities(struct()) :: capabilities()
  def capabilities(struct)

  @doc """
  Gets the secrets of the given hub.
  """
  @spec get_secrets(struct()) :: list(Secret.t())
  def get_secrets(struct)

  @doc """
  Creates a secret of  the given hub.
  """
  @spec create_secret(struct(), Secret.t()) :: :ok | {:error, changeset_errors()}
  def create_secret(struct, secret)
end
