defmodule Livebook.Delta.Operation do
  @moduledoc false

  # An peration represents an atomic change applicable to a text.
  #
  # For plain-text (our use case) an operation can be either of:
  #
  # * `{:insert, string}` - insert the given text at the current position
  # * `{:retain, length}` - preserve the given number of characters (effectively moving the cursor)
  # * `{:delete, number}` - delete the given number of characters starting from the current position

  import Kernel, except: [length: 1]

  @type t :: insert | retain | delete

  @type insert :: {:insert, String.t()}
  @type retain :: {:retain, non_neg_integer()}
  @type delete :: {:delete, non_neg_integer()}

  @type compressed_t :: String.t() | non_neg_integer() | neg_integer()

  @spec insert(String.t()) :: t()
  def insert(string), do: {:insert, string}

  @spec insert(non_neg_integer()) :: t()
  def retain(length), do: {:retain, length}

  @spec delete(non_neg_integer()) :: t()
  def delete(length), do: {:delete, length}

  @doc """
  Returns length of text affected by a given operation.
  """
  @spec length(t()) :: non_neg_integer()
  def length({:insert, string}), do: String.length(string)
  def length({:retain, length}), do: length
  def length({:delete, length}), do: length

  @doc """
  Splits the given operation into two at the specified offset.
  """
  @spec split_at(t(), non_neg_integer()) :: {t(), t()}
  def split_at(op, position)

  def split_at({:insert, string}, position) do
    {part_one, part_two} = String.split_at(string, position)
    {insert(part_one), insert(part_two)}
  end

  def split_at({:retain, length}, position) do
    {retain(position), retain(length - position)}
  end

  def split_at({:delete, length}, position) do
    {delete(position), delete(length - position)}
  end

  @doc """
  Converts the given operation to a basic type uniquely identifying it.
  """
  @spec to_compressed(t()) :: compressed_t()
  def to_compressed({:insert, string}), do: string
  def to_compressed({:retain, length}), do: length
  def to_compressed({:delete, length}), do: -length

  @doc """
  Converts the given basic type to the corresponding operation.
  """
  @spec from_compressed(compressed_t()) :: t()
  def from_compressed(string) when is_binary(string), do: {:insert, string}
  def from_compressed(length) when is_integer(length) and length >= 0, do: {:retain, length}
  def from_compressed(length) when is_integer(length) and length < 0, do: {:delete, -length}

  @doc """
  Modifies the given operation lists, so that their heads
  have the same operation length.

  ## Examples

      iex> left = [{:insert, "cat"}]
      iex> right = [{:retain, 2}, {:delete, 2}]
      iex> Livebook.Delta.Operation.align_heads(left, right)
      {
        [{:insert, "ca"}, {:insert, "t"}],
        [{:retain, 2}, {:delete, 2}]
      }
  """
  @spec align_heads(list(t()), list(t())) :: {list(t()), list(t())}
  def align_heads([head_a | tail_a], [head_b | tail_b]) do
    len_a = length(head_a)
    len_b = length(head_b)

    cond do
      len_a > len_b ->
        {left_a, right_a} = split_at(head_a, len_b)
        {[left_a, right_a | tail_a], [head_b | tail_b]}

      len_a < len_b ->
        {left_b, right_b} = split_at(head_b, len_a)
        {[head_a | tail_a], [left_b, right_b | tail_b]}

      true ->
        {[head_a | tail_a], [head_b | tail_b]}
    end
  end
end
