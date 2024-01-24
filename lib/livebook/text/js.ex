defmodule Livebook.Text.JS do
  # String operations replicating the JavaScript behaviour.
  #
  # JavaScript uses UTF-16 encoding, in which every character is stored
  # as either one or two 16-bit code units. String length is the number
  # of these units. This also impacts position-based functions such as
  # `String.slice`.
  #
  # Functions in this module work with regular Elixir strings, but
  # use lengths and offsets according to the above definition.

  import Kernel, except: [length: 1]

  @doc """
  Returns the length of the given string.

  The length is defined as the number of UTF-16 code units used to
  encode the string.
  """
  @spec length(String.t()) :: non_neg_integer()
  def length(string) do
    do_length(string, 0)
  end

  defp do_length(<<>>, length), do: length

  defp do_length(<<codepoint::utf8, rest::binary>>, length) do
    do_length(rest, length + num_code_units(codepoint))
  end

  @doc """
  Returns a substring starting at the offset `start`.

  See `slice/3` for slicing a specific length.
  """
  @spec slice(String.t(), non_neg_integer()) :: String.t()
  def slice(string, start) do
    drop(string, start)
  end

  @doc """
  Returns a substring starting at the offset `start` and having the
  given `length`.
  """
  @spec slice(String.t(), non_neg_integer(), non_neg_integer()) :: String.t()
  def slice(string, start, length) do
    string |> drop(start) |> take(length)
  end

  @doc """
  Splits a string into two parts at the specified offset.
  """
  @spec split_at(String.t(), non_neg_integer()) :: {String.t(), String.t()}
  def split_at(string, position) do
    offset = byte_offset(string, position)
    <<left::binary-size(offset), right::binary>> = string
    {left, right}
  end

  defp drop(<<codepoint::utf8, rest::binary>>, n) when n > 0 do
    drop(rest, n - num_code_units(codepoint))
  end

  defp drop(string, 0), do: string

  defp take(string, n) do
    offset = byte_offset(string, n)
    binary_part(string, 0, offset)
  end

  defp byte_offset(string, length), do: byte_offset(string, length, 0)

  defp byte_offset(_string, 0, offset), do: offset

  defp byte_offset(<<codepoint::utf8, rest::binary>> = string, length, offset) when length > 0 do
    num_bytes = byte_size(string) - byte_size(rest)
    byte_offset(rest, length - num_code_units(codepoint), offset + num_bytes)
  end

  defp num_code_units(codepoint) do
    num_utf16_bytes = byte_size(<<codepoint::utf16>>)
    div(num_utf16_bytes, 2)
  end

  @doc """
  Maps `column` pointing to a UTF-16 code unit in `line` to the
  corresponding grapheme it is a part of.
  """
  @spec js_column_to_elixir(pos_integer(), String.t()) :: pos_integer()
  def js_column_to_elixir(column, line) when column > 0 do
    do_js_column_to_elixir(line, column, 0)
  end

  defp do_js_column_to_elixir(<<>>, _column, num_graphemes), do: num_graphemes

  defp do_js_column_to_elixir(line, column, num_graphemes) do
    {grapheme, line} = String.next_grapheme(line)
    length = length(grapheme)

    if column <= length do
      num_graphemes + 1
    else
      do_js_column_to_elixir(line, column - length, num_graphemes + 1)
    end
  end

  @doc """
  Maps `column` pointing to a grapheme in `line` to the first UTF-16
  code unit in that grapheme.
  """
  @spec elixir_column_to_js(pos_integer(), String.t()) :: pos_integer()
  def elixir_column_to_js(column, line) when column > 0 do
    do_elixir_column_to_js(line, column, 0)
  end

  defp do_elixir_column_to_js(_line, 1, length), do: length + 1

  defp do_elixir_column_to_js(line, column, length) do
    {grapheme, line} = String.next_grapheme(line)
    do_elixir_column_to_js(line, column - 1, length + length(grapheme))
  end
end
