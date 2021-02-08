defprotocol LiveBook.Runtime do
  @moduledoc false

  # This protocol defines an interface that every runtime should adhere to,
  # so that other modules can rely on the abstraction.
  #
  # Runtime is essentially an Elixir node along with metadata.
  # Runtime types differ in how the node is created and managed
  # (see `Runtime.Standalone` and `Runtime.Attached`).
  #
  # See `Remote` for more details on how the node is used.

  @doc """
  Returns the Elixir node represented by the given runtime.
  """
  @spec get_node(t()) :: node()
  def get_node(runtime)

  @doc """
  Disposes of the underlying node by reverting whatever
  was done during initialization.
  """
  @spec disconnect(t()) :: :ok
  def disconnect(runtime)
end
