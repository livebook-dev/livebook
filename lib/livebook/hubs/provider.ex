defprotocol Livebook.Hubs.Provider do
  @moduledoc false

  @doc """
  Normalize given struct to `Livebook.Hubs.Metadata` struct.
  """
  @spec normalize(struct()) :: Livebook.Hubs.Metadata.t()
  def normalize(struct)

  @doc """
  Loads fields into given struct.
  """
  @spec load(struct(), map() | keyword()) :: struct()
  def load(struct, fields)

  @doc """
  Gets the type from struct.
  """
  @spec type(struct()) :: String.t()
  def type(struct)

  @doc """
  Checks if the given struct can connect.
  """
  @spec connectable?(struct()) :: boolean()
  def connectable?(struct)

  @doc """
  Gets or register the PID of the given struct on the dynamic supervisor.
  """
  @spec connect(struct()) :: :noop | pid()
  def connect(struct)
end
