defmodule Livebook.Utils.ANSI.Modifier do
  @moduledoc false

  defmacro defmodifier(modifier, code, terminator \\ "m") do
    quote bind_quoted: [modifier: modifier, code: code, terminator: terminator] do
      defp ansi_prefix_to_modifier(unquote("[#{code}#{terminator}") <> rest) do
        {:ok, unquote(modifier), rest}
      end
    end
  end
end

defmodule Livebook.Utils.ANSI do
  @moduledoc false

  import Livebook.Utils.ANSI.Modifier

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
  """
  @spec parse_ansi_string(String.t()) :: list({list(modifier()), String.t()})
  def parse_ansi_string(string) do
    [head | ansi_prefixed_strings] = String.split(string, "\e")

    # Each part has the form of {modifiers, string}
    {tail_parts, _} =
      Enum.map_reduce(ansi_prefixed_strings, %{}, fn string, modifiers ->
        {modifiers, rest} =
          case ansi_prefix_to_modifier(string) do
            {:ok, modifier, rest} ->
              modifiers = add_modifier(modifiers, modifier)
              {modifiers, rest}

            {:error, _rest} ->
              {modifiers, "\e" <> string}
          end

        {{Map.to_list(modifiers), rest}, modifiers}
      end)

    parts = [{[], head} | tail_parts]

    parts
    |> Enum.reject(fn {_modifiers, string} -> string == "" end)
    |> merge_adjacent_parts([])
  end

  defp merge_adjacent_parts([], acc), do: Enum.reverse(acc)

  defp merge_adjacent_parts([{modifiers, string1}, {modifiers, string2} | parts], acc) do
    merge_adjacent_parts([{modifiers, string1 <> string2} | parts], acc)
  end

  defp merge_adjacent_parts([part | parts], acc) do
    merge_adjacent_parts(parts, [part | acc])
  end

  # Below goes a number of `ansi_prefix_to_modifier` function definitions,
  # that take a string like "[32msomething" (starting with ANSI code without the leading "\e")
  # and parse the prefix into the corresponding modifier.
  # The function returns either {:ok, modifier, rest} or {:error, rest}

  defmodifier(:reset, 0)

  # When the code is missing (i.e., "\e[m"), it is 0 for reset.
  defmodifier(:reset, "")

  @colors [:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white]

  for {color, index} <- Enum.with_index(@colors) do
    defmodifier({:foreground_color, color}, 30 + index)
    defmodifier({:background_color, color}, 40 + index)
    defmodifier({:foreground_color, :"light_#{color}"}, 90 + index)
    defmodifier({:background_color, :"light_#{color}"}, 100 + index)
  end

  defmodifier({:foreground_color, :reset}, 39)
  defmodifier({:background_color, :reset}, 49)

  defmodifier({:font_weight, :bold}, 1)
  defmodifier({:font_weight, :light}, 2)
  defmodifier({:font_style, :italic}, 3)
  defmodifier({:text_decoration, :underline}, 4)
  defmodifier({:text_decoration, :line_through}, 9)
  defmodifier({:font_weight, :reset}, 22)
  defmodifier({:font_style, :reset}, 23)
  defmodifier({:text_decoration, :reset}, 24)
  defmodifier({:text_decoration, :overline}, 53)
  defmodifier({:text_decoration, :reset}, 55)

  defp ansi_prefix_to_modifier("[38;5;" <> string) do
    with {:ok, color, rest} <- bit8_prefix_to_color(string) do
      {:ok, {:foreground_color, color}, rest}
    end
  end

  defp ansi_prefix_to_modifier("[48;5;" <> string) do
    with {:ok, color, rest} <- bit8_prefix_to_color(string) do
      {:ok, {:background_color, color}, rest}
    end
  end

  # "\e(B" is RFC1468's switch to ASCII character set and can be ignored. This
  # can appear even when JIS character sets aren't in use.
  defp ansi_prefix_to_modifier("(B" <> rest) do
    {:ok, :ignored, rest}
  end

  defp bit8_prefix_to_color(string) do
    case Integer.parse(string) do
      {n, "m" <> rest} when n in 0..255 ->
        color = color_from_code(n)
        {:ok, color, rest}

      _ ->
        {:error, string}
    end
  end

  ignored_codes = [5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 25, 27, 51, 52, 54]

  for code <- ignored_codes do
    defmodifier(:ignored, code)
  end

  defmodifier(:ignored, 1, "A")
  defmodifier(:ignored, 1, "B")
  defmodifier(:ignored, 1, "C")
  defmodifier(:ignored, 1, "D")
  defmodifier(:ignored, 2, "J")
  defmodifier(:ignored, 2, "K")
  defmodifier(:ignored, "", "H")

  defp ansi_prefix_to_modifier(string), do: {:error, string}

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

  defp add_modifier(modifiers, :ignored), do: modifiers
  defp add_modifier(_modifiers, :reset), do: %{}
  defp add_modifier(modifiers, {key, :reset}), do: Map.delete(modifiers, key)
  defp add_modifier(modifiers, {key, value}), do: Map.put(modifiers, key, value)
end
