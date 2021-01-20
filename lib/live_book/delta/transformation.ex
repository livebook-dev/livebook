defmodule LiveBook.Delta.Transformation do
  alias LiveBook.Delta
  alias LiveBook.Delta.{Operation, Iterator}

  @type priority :: :left | :right

  def transform(left, right, priority) do
    {left.ops, right.ops}
    |> Iterator.next()
    |> do_transform(priority, Delta.new())
    |> Delta.trim()
  end

  defp do_transform({{_, _}, {nil, _}}, _, result) do
    result
  end

  defp do_transform({{nil, _}, {op_b, remainder_b}}, _, result) do
    Enum.reduce([op_b | remainder_b], result, &Delta.append(&2, &1))
  end

  defp do_transform(
         {{{:insert, _} = ins_a, remainder_a}, {{:insert, _} = ins_b, remainder_b}},
         :left,
         result
       ) do
    retain = make_retain(ins_a)

    {remainder_a, [ins_b | remainder_b]}
    |> Iterator.next()
    |> do_transform(:left, Delta.append(result, retain))
  end

  defp do_transform(
         {{{:insert, _} = ins_a, remainder_a}, {{:insert, _} = ins_b, remainder_b}},
         :right,
         result
       ) do
    {[ins_a | remainder_a], remainder_b}
    |> Iterator.next()
    |> do_transform(:right, Delta.append(result, ins_b))
  end

  defp do_transform(
         {{{:insert, _} = ins, remainder_a}, {{:retain, _} = ret, remainder_b}},
         priority,
         result
       ) do
    retain = make_retain(ins)

    {remainder_a, [ret | remainder_b]}
    |> Iterator.next()
    |> do_transform(priority, Delta.append(result, retain))
  end

  defp do_transform(
         {{{:insert, _} = ins, remainder_a}, {{:delete, _} = del, remainder_b}},
         priority,
         result
       ) do
    retain = make_retain(ins)

    {remainder_a, [del | remainder_b]}
    |> Iterator.next()
    |> do_transform(priority, Delta.append(result, retain))
  end

  defp do_transform(
         {{{:delete, _} = del, remainder_a}, {{:insert, _} = ins, remainder_b}},
         priority,
         result
       ) do
    {[del | remainder_a], remainder_b}
    |> Iterator.next()
    |> do_transform(priority, Delta.append(result, ins))
  end

  defp do_transform(
         {{{:delete, _}, remainder_a}, {{:retain, _}, remainder_b}},
         priority,
         result
       ) do
    {remainder_a, remainder_b}
    |> Iterator.next()
    |> do_transform(priority, result)
  end

  defp do_transform(
         {{{:delete, _}, remainder_a}, {{:delete, _}, remainder_b}},
         priority,
         result
       ) do
    {remainder_a, remainder_b}
    |> Iterator.next()
    |> do_transform(priority, result)
  end

  defp do_transform(
         {{{:retain, _} = ret, remainder_a}, {{:insert, _} = ins, remainder_b}},
         priority,
         result
       ) do
    {[ret | remainder_a], remainder_b}
    |> Iterator.next()
    |> do_transform(priority, Delta.append(result, ins))
  end

  defp do_transform(
         {{{:retain, _} = ret_a, remainder_a}, {{:retain, _}, remainder_b}},
         priority,
         result
       ) do
    retain = make_retain(ret_a)

    {remainder_a, remainder_b}
    |> Iterator.next()
    |> do_transform(priority, Delta.append(result, retain))
  end

  defp do_transform(
         {{{:retain, _}, remainder_a}, {{:delete, _} = del, remainder_b}},
         priority,
         result
       ) do
    {remainder_a, remainder_b}
    |> Iterator.next()
    |> do_transform(priority, Delta.append(result, del))
  end

  defp make_retain(op) do
    op
    |> Operation.length()
    |> Operation.retain()
  end
end
