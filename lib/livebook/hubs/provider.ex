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
end
