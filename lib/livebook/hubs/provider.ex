defprotocol Livebook.Hubs.Provider do
  @moduledoc false

  @type t :: %{
          required(:__struct__) => module(),
          required(:id) => String.t(),
          optional(any()) => any()
        }

  @doc """
  Normalize given struct to `Livebook.Hubs.Metadata` struct.
  """
  @spec normalize(t()) :: Livebook.Hubs.Metadata.t()
  def normalize(struct)

  @doc """
  Loads fields into given struct.
  """
  @spec load(t(), map() | keyword()) :: t()
  def load(struct, fields)

  @doc """
  Gets the type from struct.
  """
  @spec type(t()) :: String.t()
  def type(struct)
end
