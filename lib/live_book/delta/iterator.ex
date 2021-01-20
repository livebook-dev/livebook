defmodule LiveBook.Delta.Iterator do
  alias LiveBook.Delta.Operation

  @typedoc """
  Individual set of operations.
  """
  @type set :: [Operation.t()]

  @typedoc """
  Two sets of operations to iterate.
  """
  @type sets :: {set, set}

  @typedoc """
  A tuple representing the new head and tail operations of the two operation
  sets being iterated over.
  """
  @type cycle :: {set_split, set_split}

  @typedoc """
  A set's next scanned full or partial operation, and its resulting tail set.
  """
  @type set_split :: {Operation.t() | nil, set}

  @doc """
  Generates next cycle by iterating over given sets of operations.
  """
  @spec next(sets) :: cycle
  def next(sets)

  def next({[], []}) do
    {{nil, []}, {nil, []}}
  end

  def next({[], [head_b | tail_b]}) do
    {{nil, []}, {head_b, tail_b}}
  end

  def next({[head_a | tail_a], []}) do
    {{head_a, tail_a}, {nil, []}}
  end

  def next({[head_a | tail_a], [head_b | tail_b]}) do
    len_a = Operation.length(head_a)
    len_b = Operation.length(head_b)

    cond do
      len_a > len_b ->
        {head_a, remainder_a} = Operation.slice(head_a, len_b)
        {{head_a, [remainder_a | tail_a]}, {head_b, tail_b}}

      len_a < len_b ->
        {head_b, remainder_b} = Operation.slice(head_b, len_a)
        {{head_a, tail_a}, {head_b, [remainder_b | tail_b]}}

      true ->
        {{head_a, tail_a}, {head_b, tail_b}}
    end
  end
end
