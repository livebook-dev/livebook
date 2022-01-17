defmodule Livebook.Utils.ANSI do
  @moduledoc false

  @type modifiers :: list(modifier())

  @type modifier ::
          {:font_weight, :bold | :light}
          | {:font_style, :italic}
          | {:text_decoration, :underline | :line_through | :overline}
          | {:foreground_color, color()}
          | {:background_color, color()}

  @type color :: basic_color() | {:grayscale24, 0..23} | {:rgb6, 0..5, 0..5, 0..5}

  @type basic_color ::
          :black
          | :red
          | :green
          | :yellow
          | :blue
          | :magenta
          | :cyan
          | :white
          | :light_black
          | :light_red
          | :light_green
          | :light_yellow
          | :light_blue
          | :light_magenta
          | :light_cyan
          | :light_white

  @doc """
  Takes a string with ANSI escape codes and parses it
  into a list of `{modifiers, string}` parts.

  Also returns the final modifiers.

  ## Options

    * `:modifiers` - a list with initial modifiers
  """
  @spec parse_ansi_string(String.t(), keyword()) :: {list({modifiers(), String.t()}), modifiers()}
  def parse_ansi_string(string, opts \\ []) do
    modifiers = Keyword.get(opts, :modifiers, [])

    [head | ansi_prefixed_strings] = String.split(string, "\e")

    # Each part has the form of {modifiers, string}
    {tail_parts, modifiers} =
      Enum.map_reduce(ansi_prefixed_strings, modifiers, fn string, modifiers ->
        {modifiers, rest} =
          case ansi_prefix_to_modifiers(string) do
            {:ok, new_modifiers, rest} ->
              modifiers = Enum.reduce(new_modifiers, modifiers, &apply_modifier(&2, &1))
              {modifiers, rest}

            :error ->
              {modifiers, "\e" <> string}
          end

        {{modifiers, rest}, modifiers}
      end)

    parts = [{[], head} | tail_parts]

    parts =
      parts
      |> Enum.reject(fn {_modifiers, string} -> string == "" end)
      |> merge_adjacent_parts([])

    {parts, modifiers}
  end

  defp merge_adjacent_parts([], acc), do: Enum.reverse(acc)

  defp merge_adjacent_parts([{modifiers, string1}, {modifiers, string2} | parts], acc) do
    merge_adjacent_parts([{modifiers, string1 <> string2} | parts], acc)
  end

  defp merge_adjacent_parts([part | parts], acc) do
    merge_adjacent_parts(parts, [part | acc])
  end

  defp ansi_prefix_to_modifiers("[1A" <> rest), do: {:ok, [:ignored], rest}
  defp ansi_prefix_to_modifiers("[1B" <> rest), do: {:ok, [:ignored], rest}
  defp ansi_prefix_to_modifiers("[1C" <> rest), do: {:ok, [:ignored], rest}
  defp ansi_prefix_to_modifiers("[1D" <> rest), do: {:ok, [:ignored], rest}
  defp ansi_prefix_to_modifiers("[2J" <> rest), do: {:ok, [:ignored], rest}
  defp ansi_prefix_to_modifiers("[2K" <> rest), do: {:ok, [:ignored], rest}
  defp ansi_prefix_to_modifiers("[H" <> rest), do: {:ok, [:ignored], rest}

  # "\e(B" is RFC1468's switch to ASCII character set and can be ignored. This
  # can appear even when JIS character sets aren't in use
  defp ansi_prefix_to_modifiers("(B" <> rest), do: {:ok, [:ignored], rest}

  defp ansi_prefix_to_modifiers("[" <> rest) do
    with [args_string, rest] <- String.split(rest, "m", parts: 2),
         {:ok, args} <- parse_ansi_args(args_string),
         {:ok, modifiers} <- ansi_args_to_modifiers(args, []) do
      {:ok, modifiers, rest}
    else
      _ -> :error
    end
  end

  defp ansi_prefix_to_modifiers(_string), do: :error

  defp parse_ansi_args(args_string) do
    args_string
    |> String.split(";")
    |> Enum.reduce_while([], fn arg, parsed ->
      case parse_ansi_arg(arg) do
        {:ok, n} -> {:cont, [n | parsed]}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      :error -> :error
      parsed -> {:ok, Enum.reverse(parsed)}
    end
  end

  defp parse_ansi_arg(""), do: {:ok, 0}

  defp parse_ansi_arg(string) do
    case Integer.parse(string) do
      {n, ""} -> {:ok, n}
      _ -> :error
    end
  end

  defp ansi_args_to_modifiers([], acc), do: {:ok, Enum.reverse(acc)}

  defp ansi_args_to_modifiers(args, acc) do
    case ansi_args_to_modifier(args) do
      {:ok, modifier, args} -> ansi_args_to_modifiers(args, [modifier | acc])
      :error -> :error
    end
  end

  @colors [:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white]

  defp ansi_args_to_modifier(args) do
    case args do
      [0 | args] ->
        {:ok, :reset, args}

      [1 | args] ->
        {:ok, {:font_weight, :bold}, args}

      [2 | args] ->
        {:ok, {:font_weight, :light}, args}

      [3 | args] ->
        {:ok, {:font_style, :italic}, args}

      [4 | args] ->
        {:ok, {:text_decoration, :underline}, args}

      [9 | args] ->
        {:ok, {:text_decoration, :line_through}, args}

      [22 | args] ->
        {:ok, {:font_weight, :reset}, args}

      [23 | args] ->
        {:ok, {:font_style, :reset}, args}

      [24 | args] ->
        {:ok, {:text_decoration, :reset}, args}

      [n | args] when n in 30..37 ->
        color = Enum.at(@colors, n - 30)
        {:ok, {:foreground_color, color}, args}

      [38, 5, bit8 | args] when bit8 in 0..255 ->
        color = color_from_code(bit8)
        {:ok, {:foreground_color, color}, args}

      [39 | args] ->
        {:ok, {:foreground_color, :reset}, args}

      [n | args] when n in 40..47 ->
        color = Enum.at(@colors, n - 40)
        {:ok, {:background_color, color}, args}

      [48, 5, bit8 | args] when bit8 in 0..255 ->
        color = color_from_code(bit8)
        {:ok, {:background_color, color}, args}

      [49 | args] ->
        {:ok, {:background_color, :reset}, args}

      [53 | args] ->
        {:ok, {:text_decoration, :overline}, args}

      [55 | args] ->
        {:ok, {:text_decoration, :reset}, args}

      [n | args] when n in 90..97 ->
        color = Enum.at(@colors, n - 90)
        {:ok, {:foreground_color, :"light_#{color}"}, args}

      [n | args] when n in 100..107 ->
        color = Enum.at(@colors, n - 100)
        {:ok, {:background_color, :"light_#{color}"}, args}

      [n | args] when n <= 107 ->
        {:ok, :ignored, args}

      _ ->
        :error
    end
  end

  defp color_from_code(code) when code in 0..7 do
    Enum.at(@colors, code)
  end

  defp color_from_code(code) when code in 8..15 do
    color = Enum.at(@colors, code - 8)
    :"light_#{color}"
  end

  defp color_from_code(code) when code in 16..231 do
    rgb_code = code - 16
    b = rgb_code |> rem(6)
    g = rgb_code |> div(6) |> rem(6)
    r = rgb_code |> div(36)

    {:rgb6, r, g, b}
  end

  defp color_from_code(code) when code in 232..255 do
    level = code - 232
    {:grayscale24, level}
  end

  defp apply_modifier(modifiers, :ignored), do: modifiers
  defp apply_modifier(_modifiers, :reset), do: []
  defp apply_modifier(modifiers, {key, :reset}), do: Keyword.delete(modifiers, key)
  defp apply_modifier(modifiers, {key, value}), do: Keyword.put(modifiers, key, value)
end
