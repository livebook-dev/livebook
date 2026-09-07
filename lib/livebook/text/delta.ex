defmodule Livebook.Text.Delta do
  # Delta is a format used to represent a set of changes introduced
  # to a text document.
  #
  # By design, delta is suitable for Operational Transformation and
  # is hence our primary building block in collaborative text editing.
  #
  # For a detailed write-up see https://quilljs.com/docs/delta and
  # https://quilljs.com/guides/designing-the-delta-format. The
  # specification covers rich-text editing, while we only need to
  # work with plain-text, so we use a subset of the specification
  # with operations listed in `Livebook.Text.Delta.Operation`.
  #
  # An implementation of the full Delta specification is available in
  # the :text_delta package (https://github.com/deltadoc/text_delta)
  # by Konstantin Kudryashov under the MIT license. This module builds
  # directly on that package, and is simplified to better fit our not
  # rich-text use case.
  #
  # ## Lengths and offsets
  #
  # Delta operations involve strings and string lengths. The exact
  # definition of these depends on the string representation in the
  # given programming language. This module is implemented for
  # compatibility with JavaScript, hence all lengths match the length
  # as defined by JavaScript (the number of UTF-16 code units).
  #
  # All places with calls to the `Livebook.Text.JS` module are the
  # ones where the implementation is affected by this distinction.

  defstruct ops: []

  alias Livebook.Text
  alias Livebook.Text.Delta
  alias Livebook.Text.Delta.Operation
  alias Livebook.Text.Delta.Transformation

  @typedoc """
  Delta carries a list of consecutive operations.

  Note that we keep the operations in reversed order for efficiency.
  """
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

    1. Delta must be *compact* - there must be no shorter equivalent
       delta.

    2. Delta must be *canonical* - there is just a single valid
       representation of the given change.

  To satisfy these constraints we follow two rules:

    1. Delete followed by insert is swapped to ensure that insert
       goes first.

    2. Operations of the same type are merged.

  """
  @spec append(t(), Operation.t()) :: t()
  def append(delta, op) do
    Map.update!(delta, :ops, &compact(&1, op))
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
  def trim(%Delta{ops: [{:retain, _} | ops]} = delta), do: %{delta | ops: ops}
  def trim(delta), do: delta

  @doc """
  Checks if the delta has no changes.
  """
  @spec empty?(t()) :: boolean()
  def empty?(delta) do
    trim(delta).ops == []
  end

  @doc """
  Returns data operations in the order in which they apply.
  """
  @spec operations(t()) :: list(Operation.t())
  def operations(delta) do
    Enum.reverse(delta.ops)
  end

  @doc """
  Converts the given delta to a compact representation, suitable for
  JSON serialization.

  ## Examples

      iex> delta = Delta.new([retain: 2, insert: "hey", delete: 3])
      iex> Livebook.Text.Delta.to_compressed(delta)
      [2, "hey", -3]

  """
  @spec to_compressed(t()) :: list(Operation.compressed_t())
  def to_compressed(delta) do
    delta.ops
    |> Enum.reverse()
    |> Enum.map(&Operation.to_compressed/1)
  end

  @doc """
  Builds a new delta from the given compact representation.

  ## Examples

      iex> delta = Livebook.Text.Delta.from_compressed([2, "hey", -3])
      iex> Livebook.Text.Delta.operations(delta)
      [retain: 2, insert: "hey", delete: 3]

  """
  @spec from_compressed(list(Operation.compressed_t())) :: t()
  def from_compressed(list) do
    ops =
      list
      |> Enum.map(&Operation.from_compressed/1)
      |> Enum.reverse()

    %Delta{ops: ops}
  end

  defdelegate transform(left, right, priority), to: Transformation

  defdelegate transform_position(delta, index), to: Transformation

  @doc """
  Returns the result of applying `delta` to `string`.
  """
  @spec apply(Delta.t(), String.t()) :: String.t()
  def apply(delta, string) do
    do_apply(operations(delta), <<>>, string)
  end

  defp do_apply([{:retain, n} | ops], result, string) do
    {left, right} = Text.JS.split_at(string, n)
    do_apply(ops, <<result::binary, left::binary>>, right)
  end

  defp do_apply([{:insert, inserted} | ops], result, string) do
    do_apply(ops, <<result::binary, inserted::binary>>, string)
  end

  defp do_apply([{:delete, n} | ops], result, string) do
    do_apply(ops, result, Text.JS.slice(string, n))
  end

  defp do_apply([], result, string) do
    <<result::binary, string::binary>>
  end

  # The Myers algorithm is quadratic in the edit distance, so we emit
  # a replace when the diff would be too expensive: for an empty string
  # (the edit distance is the whole other string), for long lines and
  # for long line lists
  @max_diff_bytes 2_000
  @max_lines 2_000

  @doc """
  Computes the difference between the given strings and returns its
  `Delta` representation.

  The difference is computed on lines and then within each changed
  block of lines, so a single long line does not affect the diff of
  the other lines. Blocks over #{@max_diff_bytes} bytes are diffed
  line by line, lines over that size are replaced as a whole, and so
  are strings with over #{@max_lines} lines. All sizes count both
  strings together.
  """
  @spec diff(String.t(), String.t()) :: Delta.t()
  def diff(string1, string2) do
    lines1 = split_lines(string1)
    lines2 = split_lines(string2)

    delta =
      if lines1 == [] or lines2 == [] or length(lines1) + length(lines2) > @max_lines do
        replace(string1, string2, Delta.new())
      else
        lines1
        |> List.myers_difference(lines2)
        |> script_to_delta(Delta.new())
      end

    Delta.trim(delta)
  end

  defp split_lines(string) do
    line_ends = for {offset, _length} <- :binary.matches(string, "\n"), do: offset + 1
    split_lines(string, 0, line_ends)
  end

  defp split_lines(string, start, [line_end | line_ends]) do
    [binary_part(string, start, line_end - start) | split_lines(string, line_end, line_ends)]
  end

  defp split_lines(string, start, []) when start == byte_size(string), do: []
  defp split_lines(string, start, []), do: [binary_part(string, start, byte_size(string) - start)]

  # Deleted lines followed by inserted lines are diffed as one block,
  # so that changes such as line splits keep the common characters.
  defp script_to_delta([{:del, deleted}, {:ins, inserted} | script], delta) do
    script_to_delta(script, diff_blocks(deleted, inserted, delta))
  end

  defp script_to_delta([{:eq, lines} | script], delta) do
    script_to_delta(script, Delta.retain(delta, lines_length(lines)))
  end

  defp script_to_delta([{:ins, lines} | script], delta) do
    script_to_delta(script, Delta.insert(delta, IO.iodata_to_binary(lines)))
  end

  defp script_to_delta([{:del, lines} | script], delta) do
    script_to_delta(script, Delta.delete(delta, lines_length(lines)))
  end

  defp script_to_delta([], delta), do: delta

  defp diff_blocks(deleted, inserted, delta) do
    string1 = IO.iodata_to_binary(deleted)
    string2 = IO.iodata_to_binary(inserted)

    if byte_size(string1) + byte_size(string2) > @max_diff_bytes do
      diff_line_pairs(deleted, inserted, delta)
    else
      diff_strings(string1, string2, delta)
    end
  end

  defp diff_line_pairs([line1 | lines1], [line2 | lines2], delta) do
    diff_line_pairs(lines1, lines2, diff_line(line1, line2, delta))
  end

  defp diff_line_pairs([], [], delta), do: delta
  defp diff_line_pairs(lines1, [], delta), do: Delta.delete(delta, lines_length(lines1))
  defp diff_line_pairs([], lines2, delta), do: Delta.insert(delta, IO.iodata_to_binary(lines2))

  defp diff_line(line1, line2, delta)
       when byte_size(line1) + byte_size(line2) > @max_diff_bytes do
    replace(line1, line2, delta)
  end

  defp diff_line(line1, line2, delta), do: diff_strings(line1, line2, delta)

  defp diff_strings(string1, string2, delta) do
    string1
    |> String.myers_difference(string2)
    |> Enum.reduce(delta, fn
      {:eq, string}, delta -> Delta.retain(delta, Text.JS.length(string))
      {:ins, string}, delta -> Delta.insert(delta, string)
      {:del, string}, delta -> Delta.delete(delta, Text.JS.length(string))
    end)
  end

  defp replace(string1, string2, delta) do
    delta
    |> Delta.delete(Text.JS.length(string1))
    |> Delta.insert(string2)
  end

  defp lines_length(lines) do
    Enum.sum_by(lines, &Text.JS.length/1)
  end
end
