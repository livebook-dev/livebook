defmodule Livebook.Delta do
  @moduledoc false

  # Delta is a format used to represent a set of changes
  # introduced to a text document.
  #
  # By design, delta is suitable for Operational Transformation
  # and is hence our primary building block in collaborative text editing.
  #
  # For a detailed write-up see https://quilljs.com/docs/delta
  # and https://quilljs.com/guides/designing-the-delta-format.
  # The specification covers rich-text editing, while we only
  # need to work with plain-text, so we use a subset of the specification
  # with operations listed in `Livebook.Delta.Operation`.
  #
  # Also see https://hexdocs.pm/text_delta/TextDelta.html
  # for a complete implementation of the Delta specification.

  defstruct ops: []

  alias Livebook.Delta
  alias Livebook.Delta.{Operation, Transformation}

  @type t :: %Delta{ops: list(Operation.t())}

  @doc """
  Creates a new delta, optionally taking a list of operations.
  """
  @spec new(list(Operation.t())) :: t()
  def new(opts \\ [])
  def new([]), do: %Delta{}
  def new(ops), do: Enum.reduce(ops, new(), &append(&2, &1))

  @doc """
  Appends a new `:insert` operation to the given delta.
  """
  @spec insert(t(), String.t()) :: t()
  def insert(delta, string) do
    append(delta, Operation.insert(string))
  end

  @doc """
  Appends a new `:retain` operation to the given delta.
  """
  @spec retain(t(), non_neg_integer()) :: t()
  def retain(delta, length) do
    append(delta, Operation.retain(length))
  end

  @doc """
  Appends a new `:delete` operation to the given delta.
  """
  @spec delete(t(), non_neg_integer()) :: t()
  def delete(delta, length) do
    append(delta, Operation.delete(length))
  end

  @doc """
  Appends an operation to the given delta.

  The specification imposes two constraints:

  1. Delta must be *compact* - there must be no shorter equivalent delta.
  2. Delta must be *canonical* - there is just a single valid representation of the given change.

  To satisfy these constraints we follow two rules:

  1. Delete followed by insert is swapped to ensure that insert goes first.
  2. Operations of the same type are merged.
  """
  @spec append(t(), Operation.t()) :: t()
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

  defp compact([{:insert, str_a} | ops_remainder], {:insert, str_b}) do
    [Operation.insert(str_a <> str_b) | ops_remainder]
  end

  defp compact([{:delete, len_a} | ops_remainder], {:delete, len_b}) do
    [Operation.delete(len_a + len_b) | ops_remainder]
  end

  defp compact(ops, new_op), do: [new_op | ops]

  @doc """
  Removes trailing retain operations from the given delta.
  """
  @spec trim(t()) :: t()
  def trim(%Delta{ops: []} = delta), do: delta

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

  @doc """
  Converts the given delta to a compact representation,
  suitable for sending over the network.

  ## Examples

      iex> delta = %Livebook.Delta{ops: [retain: 2, insert: "hey", delete: 3]}
      iex> Livebook.Delta.to_compressed(delta)
      [2, "hey", -3]
  """
  @spec to_compressed(t()) :: list(Operation.compressed_t())
  def to_compressed(delta) do
    Enum.map(delta.ops, &Operation.to_compressed/1)
  end

  @doc """
  Builds a new delta from the given compact representation.

  ## Examples

      iex> Livebook.Delta.from_compressed([2, "hey", -3])
      %Livebook.Delta{ops: [retain: 2, insert: "hey", delete: 3]}
  """
  @spec from_compressed(list(Operation.compressed_t())) :: t()
  def from_compressed(list) do
    list
    |> Enum.map(&Operation.from_compressed/1)
    |> new()
  end

  defdelegate transform(left, right, priority), to: Transformation
end
