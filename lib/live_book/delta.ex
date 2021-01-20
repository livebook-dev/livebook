defmodule LiveBook.Delta do
  defstruct ops: []

  alias LiveBook.Delta.{Operation, Transformation}

  @type t :: %__MODULE__{
          ops: list(Operation.t())
        }

  @doc """
  Creates a new delta, optionally taking a list of operations.
  """
  def new(opts \\ [])
  def new([]), do: %__MODULE__{}
  def new(ops), do: Enum.reduce(ops, new(), &append(&2, &1))

  def insert(delta, text) do
    append(delta, Operation.insert(text))
  end

  def retain(delta, length) do
    append(delta, Operation.retain(length))
  end

  def delete(delta, length) do
    append(delta, Operation.delete(length))
  end

  def append(delta, op) do
    Map.update!(delta, :ops, fn ops ->
      ops
      |> Enum.reverse()
      |> compact(op)
      |> Enum.reverse()
    end)
  end

  defp compact(ops, {:insert, ""}), do: ops
  defp compact(ops, {:retain, 0}), do: ops
  defp compact(ops, {:delete, 0}), do: ops
  defp compact([], new_op), do: [new_op]

  defp compact([{:delete, _} = del | ops_remainder], {:insert, _} = ins) do
    ops_remainder
    |> compact(ins)
    |> compact(del)
  end

  defp compact([{:retain, len_a} | ops_remainder], {:retain, len_b}) do
    [Operation.retain(len_a + len_b) | ops_remainder]
  end

  defp compact([{:insert, text_a} | ops_remainder], {:insert, text_b}) do
    [Operation.insert(text_a <> text_b) | ops_remainder]
  end

  defp compact([{:delete, len_a} | ops_remainder], {:delete, len_b}) do
    [Operation.delete(len_a + len_b) | ops_remainder]
  end

  defp compact(ops, new_op), do: [new_op | ops]

  def trim(%TextDelta{ops: []} = delta), do: delta

  def trim(delta) do
    case List.last(delta.ops) do
      {:retain, _} ->
        Map.update!(delta, :ops, fn ops ->
          ops |> Enum.reverse() |> tl() |> Enum.reverse()
        end)

      _ ->
        delta
    end
  end

  def apply_to_text(delta, text) do
    apply_ops_to_text(delta.ops, text)
  end

  defp apply_ops_to_text([], text), do: text

  defp apply_ops_to_text([{:retain, n} | ops], text) do
    {left, right} = String.split_at(text, n)
    left <> apply_ops_to_text(ops, right)
  end

  defp apply_ops_to_text([{:insert, inserted} | ops], text) do
    inserted <> apply_ops_to_text(ops, text)
  end

  defp apply_ops_to_text([{:delete, n} | ops], text) do
    apply_ops_to_text(ops, String.slice(text, n..-1))
  end

  def to_compressed(delta) do
    Enum.map(delta.ops, &Operation.to_compressed/1)
  end

  def from_compressed(list) do
    list
    |> Enum.map(&Operation.from_compressed/1)
    |> new()
  end

  defdelegate transform(left, right, priority), to: Transformation
end
