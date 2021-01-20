defmodule LiveBook.Delta.Operation do
  @type t :: insert | retain | delete

  @type insert :: {:insert, String.t()}
  @type retain :: {:retain, non_neg_integer()}
  @type delete :: {:delete, non_neg_integer()}

  def insert(text), do: {:insert, text}

  def retain(length), do: {:retain, length}

  def delete(length), do: {:delete, length}

  def type({type, _}), do: type

  def length({:insert, text}), do: String.length(text)
  def length({:retain, length}), do: length
  def length({:delete, length}), do: length

  def slice(op, idx)

  def slice({:insert, text}, idx) do
    {part_one, part_two} = String.split_at(text, idx)
    {insert(part_one), insert(part_two)}
  end

  def slice({:retain, length}, idx) do
    {retain(idx), retain(length - idx)}
  end

  def slice({:delete, length}, idx) do
    {delete(idx), delete(length - idx)}
  end

  def to_compressed({:insert, text}), do: text
  def to_compressed({:retain, length}), do: length
  def to_compressed({:delete, length}), do: -length

  def from_compressed(text) when is_binary(text), do: {:insert, text}
  def from_compressed(length) when is_integer(length) and length >= 0, do: {:retain, length}
  def from_compressed(length) when is_integer(length) and length < 0, do: {:delete, -length}
end
