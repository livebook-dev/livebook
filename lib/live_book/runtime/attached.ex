defmodule LiveBook.Runtime.Attached do
  @moduledoc false

  # Represents an Elixir node managed externally.
  #
  # Such node must be already started and available,
  # LiveBook doesn't manage its lifetime in any way.
  # The node can be an oridinary Elixir runtime,
  # a mix project shell, a running release or anything else.

  defstruct [:node]

  @type t :: %__MODULE__{
          node: node()
        }

  @doc """
  Initializes the runtime by checking if the given node is reachable.
  """
  @spec init(node()) :: {:ok, t()} | {:error, :unreachable}
  def init(node) do
    case Node.ping(node) do
      :pong ->
        {:ok, %__MODULE__{node: node}}

      :pang ->
        {:error, :unreachable}
    end
  end
end

defimpl LiveBook.Runtime, for: LiveBook.Runtime.Attached do
  def get_node(runtime), do: runtime.node

  # As we don't manage the node, so there's nothing to do.
  def disconnect(_runtime), do: :ok
end
