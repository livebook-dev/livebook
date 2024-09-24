defmodule Livebook.Text.Delta.Transformation do
  # Implementation of the Operational Transformation algorithm for
  # deltas.
  #
  # The transformation allows for conflict resolution in concurrent
  # editing. Consider delta `Oa` and delta `Ob` that occurred at the
  # same time against the same text state `S`. The resulting new text
  # states are `S ∘ Oa` and `S ∘ Ob` respectively. Now for each text
  # state we would like to apply the other delta, so that both texts
  # converge to the same state, that is:
  #
  #     S ∘ Oa ∘ transform(Oa, Ob) = S ∘ Ob ∘ transform(Ob, Oa)
  #
  # That's the high-level idea. To actually achieve convergence we
  # have to introduce a linear order of operations. This way we can
  # resolve conflicts, for example, if two deltas insert a text at
  # the same position, we have to unambiguously determine which takes
  # precedence. A reasonable solution is to have a server process where
  # all the clients send deltas, as it naturally imposes the necessary
  # ordering.

  alias Livebook.Text.Delta
  alias Livebook.Text.Delta.Operation

  @type priority :: :left | :right

  @doc """
  Transforms `right` delta against the `left` delta.

  Assuming both deltas represent changes applied to the same document
  state, this operation results in modified `right` delta that represents
  effectively the same changes (preserved intent), but works on the
  document with `left` delta already applied.

  The `priority` indicates which delta is considered to have happened
  first and is used for conflict resolution.
  """
  @spec transform(Delta.t(), Delta.t(), priority()) :: Delta.t()
  def transform(left, right, priority) do
    do_transform(Delta.operations(left), Delta.operations(right), priority, Delta.new())
    |> Delta.trim()
  end

  defp do_transform(_ops_a, [] = _ops_b, _priority, result) do
    result
  end

  defp do_transform([], ops_b, _priority, result) do
    Enum.reduce(ops_b, result, &Delta.append(&2, &1))
  end

  defp do_transform([{:insert, _} | _] = ops_a, [{:insert, _} | _] = ops_b, :left, result) do
    [ins_a | remainder_a] = ops_a
    retain = make_retain(ins_a)
    do_transform(remainder_a, ops_b, :left, Delta.append(result, retain))
  end

  defp do_transform([{:insert, _} | _] = ops_a, [{:insert, _} | _] = ops_b, :right, result) do
    [ins_b | remainder_b] = ops_b
    do_transform(ops_a, remainder_b, :right, Delta.append(result, ins_b))
  end

  defp do_transform([{:insert, _} | _] = ops_a, [{:retain, _} | _] = ops_b, priority, result) do
    [ins_a | remainder_a] = ops_a
    retain = make_retain(ins_a)
    do_transform(remainder_a, ops_b, priority, Delta.append(result, retain))
  end

  defp do_transform([{:insert, _} | _] = ops_a, [{:delete, _} | _] = ops_b, priority, result) do
    [ins_a | remainder_a] = ops_a
    retain = make_retain(ins_a)
    do_transform(remainder_a, ops_b, priority, Delta.append(result, retain))
  end

  defp do_transform([{:delete, _} | _] = ops_a, [{:insert, _} | _] = ops_b, priority, result) do
    [ins_b | remainder_b] = ops_b
    do_transform(ops_a, remainder_b, priority, Delta.append(result, ins_b))
  end

  defp do_transform([{:delete, _} | _] = ops_a, [{:retain, _} | _] = ops_b, priority, result) do
    {[_del_a | remainder_a], [_ret_b | remainder_b]} = Operation.align_heads(ops_a, ops_b)
    do_transform(remainder_a, remainder_b, priority, result)
  end

  defp do_transform([{:delete, _} | _] = ops_a, [{:delete, _} | _] = ops_b, priority, result) do
    {[_del_a | remainder_a], [_del_b | remainder_b]} = Operation.align_heads(ops_a, ops_b)
    do_transform(remainder_a, remainder_b, priority, result)
  end

  defp do_transform([{:retain, _} | _] = ops_a, [{:insert, _} | _] = ops_b, priority, result) do
    [ins_b | remainder_b] = ops_b
    do_transform(ops_a, remainder_b, priority, Delta.append(result, ins_b))
  end

  defp do_transform([{:retain, _} | _] = ops_a, [{:retain, _} | _] = ops_b, priority, result) do
    {[ret | remainder_a], [ret | remainder_b]} = Operation.align_heads(ops_a, ops_b)
    do_transform(remainder_a, remainder_b, priority, Delta.append(result, ret))
  end

  defp do_transform([{:retain, _} | _] = ops_a, [{:delete, _} | _] = ops_b, priority, result) do
    {[_ret_a | remainder_a], [del_b | remainder_b]} = Operation.align_heads(ops_a, ops_b)
    do_transform(remainder_a, remainder_b, priority, Delta.append(result, del_b))
  end

  defp make_retain(op) do
    op
    |> Operation.length()
    |> Operation.retain()
  end

  @doc """
  Transforms `index` against `delta`.

  The primary usage of this function is to transform text selections.
  This transformation maps the position into a matching location in
  any text that this delta would be applied to.

  Note that OT guarantees documents to converge, however transforming
  positions does not provide the same guarantee. In practice, convergence
  is achieved most of the time, except certain edge cases where inserts
  and deletions happen around the transformed position. For more context
  see [this writeup](https://marijnhaverbeke.nl/blog/collaborative-editing-cm.html#position-mapping).

  In the implementation, one decision we need to make is whether the
  position should shift when an insert happens at that position. Since
  either way we do not get convergence in all cases, we can choose any
  behaviour and we choose no to shift the position. With this variant
  replacements adjacent to the transformed position do not move the
  position across the replacement, which is a more intuitive behaviour.
  """
  @spec transform_position(Delta.t(), non_neg_integer()) :: non_neg_integer()
  def transform_position(delta, index) do
    do_transform_position(Delta.operations(delta), index, 0)
  end

  defp do_transform_position([], index, _offset), do: index
  defp do_transform_position(_ops, index, offset) when offset >= index, do: index

  defp do_transform_position([{:delete, length} | ops], index, offset) do
    index = index - min(length, index - offset)
    do_transform_position(ops, index, offset)
  end

  defp do_transform_position([{:insert, _} = op | ops], index, offset) do
    length = Operation.length(op)
    do_transform_position(ops, index + length, offset + length)
  end

  defp do_transform_position([{:retain, length} | ops], index, offset) do
    do_transform_position(ops, index, offset + length)
  end
end
