defmodule Livebook.JSInterop do
  @moduledoc false

  alias Livebook.Delta

  @doc """
  Returns the result of applying `delta` to `string`.

  The delta operation lenghts (retain, delete) are treated
  such that they match the JavaScript strings behavior.

  JavaScript uses UTF-16 encoding, in which every character is stored
  as either one or two 16-bit code units. JS treats the number of units
  as string length and this also impacts position-based functions like `String.slice`.
  To match this behavior we first convert normal UTF-8 string
  into a list of UTF-16 code points, then apply the delta to this list
  and finally convert back to a UTF-8 string.
  """
  @spec apply_delta_to_string(Delta.t(), String.t()) :: String.t()
  def apply_delta_to_string(delta, string) do
    code_units = string_to_utf16_code_units(string)

    delta.ops
    |> apply_to_code_units(code_units)
    |> utf16_code_units_to_string()
  end

  defp apply_to_code_units([], code_units), do: code_units

  defp apply_to_code_units([{:retain, n} | ops], code_units) do
    {left, right} = Enum.split(code_units, n)
    left ++ apply_to_code_units(ops, right)
  end

  defp apply_to_code_units([{:insert, inserted} | ops], code_units) do
    string_to_utf16_code_units(inserted) ++ apply_to_code_units(ops, code_units)
  end

  defp apply_to_code_units([{:delete, n} | ops], code_units) do
    apply_to_code_units(ops, Enum.slice(code_units, n..-1))
  end

  @doc """
  Returns a column number in the Elixir string corresponding to
  the given column interpreted in terms of UTF-16 code units as
  JavaScript does.
  """
  @spec convert_column_to_elixir(pos_integer(), String.t()) :: pos_integer()
  def convert_column_to_elixir(column, line) do
    line
    |> string_to_utf16_code_units()
    |> Enum.take(column - 1)
    |> utf16_code_units_to_string()
    |> String.length()
    |> Kernel.+(1)
  end

  # ---

  defp string_to_utf16_code_units(string) do
    string
    |> :unicode.characters_to_binary(:utf8, :utf16)
    |> utf16_binary_to_code_units([])
    |> Enum.reverse()
  end

  defp utf16_binary_to_code_units(<<>>, code_units), do: code_units

  defp utf16_binary_to_code_units(<<code_unit::size(16), rest::binary>>, code_units) do
    utf16_binary_to_code_units(rest, [code_unit | code_units])
  end

  defp utf16_code_units_to_string(code_units) do
    code_units
    |> Enum.reverse()
    |> code_units_to_utf16_binary(<<>>)
    |> :unicode.characters_to_binary(:utf16, :utf8)
  end

  defp code_units_to_utf16_binary([], utf16_binary), do: utf16_binary

  defp code_units_to_utf16_binary([code_unit | code_units], utf16_binary) do
    code_units_to_utf16_binary(code_units, <<code_unit::size(16), utf16_binary::binary>>)
  end
end
