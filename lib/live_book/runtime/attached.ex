defmodule LiveBook.Runtime.Attached do
  defstruct [:node]

  @type t :: %__MODULE__{
          node: node()
        }

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

  def disconnect(_runtime), do: :ok
end
