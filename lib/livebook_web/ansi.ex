defmodule Livebook.ANSI.Modifier do
  @moduledoc false

  defmacro defmodifier(modifier, code, terminator \\ "m") do
    quote bind_quoted: [modifier: modifier, code: code, terminator: terminator] do
      defp ansi_prefix_to_modifier(unquote("#{code}#{terminator}") <> rest) do
        {:ok, unquote(modifier), rest}
      end
    end
  end
end

defmodule LivebookWeb.ANSI do
  @moduledoc false

  import Livebook.ANSI.Modifier

  # modifier ::
  #   :reset
  #   | {:font_weight, :bold | :light | :reset}
  #   | {:font_style, :italic | :reset}
  #   | {:text_decoration, :underline | :line_through | :overline | :reset}
  #   | {:foreground_color, color() | :reset}
  #   | {:background_color, color() | :reset}
  #   | :ignored

  # color :: atom() | {:grayscale24, 0..23} | {:rgb6, 0..5, 0..5, 0..5}

  @doc """
  Takes a string with ANSI escape codes and build a HTML safe string
  with `span` tags having classes corresponding to the escape codes.

  Any HTML in the string is escaped.

  ## Options

  * `:renderer` - a function used to render styled HTML content.
    The function receives HTML styles string and HTML-escaped content (iodata).
    By default the renderer wraps the whole content in a single `<span>` tag with the given style.
    Note that the style may be an empty string for plain text.
  """
  @spec ansi_string_to_html(String.t(), keyword()) :: Phoenix.HTML.safe()
  def ansi_string_to_html(string, opts \\ []) do
    renderer = Keyword.get(opts, :renderer, &default_renderer/2)

    [head | ansi_prefixed_strings] = String.split(string, "\e[")

    {:safe, head_html} = Phoenix.HTML.html_escape(head)
    head_html = renderer.("", head_html)

    # Each pair has the form of {modifiers, html_content}
    {pairs, _} =
      Enum.map_reduce(ansi_prefixed_strings, %{}, fn string, modifiers ->
        {modifiers, rest} =
          case ansi_prefix_to_modifier(string) do
            {:ok, modifier, rest} ->
              modifiers = add_modifier(modifiers, modifier)
              {modifiers, rest}

            {:error, _rest} ->
              {modifiers, "\e[" <> string}
          end

        {:safe, content} = Phoenix.HTML.html_escape(rest)
        {{modifiers, content}, modifiers}
      end)

    pairs = Enum.filter(pairs, fn {_modifiers, content} -> content not in ["", []] end)

    tail_html = pairs_to_html(pairs, renderer)

    Phoenix.HTML.raw([head_html, tail_html])
  end

  # Below goes a number of `ansi_prefix_to_modifier` function definitions,
  # that take a string like "32msomething" (starting with ANSI code without the leading "\e[")
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

  defp ansi_prefix_to_modifier("38;5;" <> string) do
    with {:ok, color, rest} <- bit8_prefix_to_color(string) do
      {:ok, {:foreground_color, color}, rest}
    end
  end

  defp ansi_prefix_to_modifier("48;5;" <> string) do
    with {:ok, color, rest} <- bit8_prefix_to_color(string) do
      {:ok, {:background_color, color}, rest}
    end
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

  # Converts a list of {modifiers, html_content} pairs
  # into HTML with appropriate styling.
  defp pairs_to_html(pairs, iodata \\ [], renderer)

  defp pairs_to_html([], iodata, _renderer), do: iodata

  defp pairs_to_html([{modifiers, content1}, {modifiers, content2} | pairs], iodata, renderer) do
    # Merge content with the same modifiers, so we don't produce unnecessary tags
    pairs_to_html([{modifiers, [content1, content2]} | pairs], iodata, renderer)
  end

  defp pairs_to_html([{modifiers, content} | pairs], iodata, renderer) do
    style = modifiers_to_css(modifiers)
    rendered = renderer.(style, content)

    pairs_to_html(pairs, [iodata, rendered], renderer)
  end

  def default_renderer("", content) do
    content
  end

  def default_renderer(style, content) do
    [~s{<span style="#{style}">}, content, ~s{</span>}]
  end

  defp modifiers_to_css(modifiers) do
    modifiers
    |> Enum.map(&modifier_to_css/1)
    |> Enum.join()
  end

  defp modifier_to_css({:font_weight, :bold}), do: "font-weight: 600;"
  defp modifier_to_css({:font_weight, :light}), do: "font-weight: 200;"

  defp modifier_to_css({:font_style, :italic}), do: "font-style: italic;"

  defp modifier_to_css({:text_decoration, :underline}), do: "text-decoration: underline;"
  defp modifier_to_css({:text_decoration, :line_through}), do: "text-decoration: line-through;"
  defp modifier_to_css({:text_decoration, :overline}), do: "text-decoration: overline;"

  defp modifier_to_css({:foreground_color, color}), do: "color: #{color_to_css(color)};"

  defp modifier_to_css({:background_color, color}),
    do: "background-color: #{color_to_css(color)};"

  defp color_to_css(:black), do: "var(--ansi-color-black)"
  defp color_to_css(:light_black), do: "var(--ansi-color-light-black)"
  defp color_to_css(:red), do: "var(--ansi-color-red)"
  defp color_to_css(:light_red), do: "var(--ansi-color-light-red)"
  defp color_to_css(:green), do: "var(--ansi-color-green)"
  defp color_to_css(:light_green), do: "var(--ansi-color-light-green)"
  defp color_to_css(:yellow), do: "var(--ansi-color-yellow)"
  defp color_to_css(:light_yellow), do: "var(--ansi-color-light-yellow)"
  defp color_to_css(:blue), do: "var(--ansi-color-blue)"
  defp color_to_css(:light_blue), do: "var(--ansi-color-light-blue)"
  defp color_to_css(:magenta), do: "var(--ansi-color-magenta)"
  defp color_to_css(:light_magenta), do: "var(--ansi-color-light-magenta)"
  defp color_to_css(:cyan), do: "var(--ansi-color-cyan)"
  defp color_to_css(:light_cyan), do: "var(--ansi-color-light-cyan)"
  defp color_to_css(:white), do: "var(--ansi-color-white)"
  defp color_to_css(:light_white), do: "var(--ansi-color-light-white)"

  defp color_to_css({:rgb6, r, g, b}) do
    r = div(255 * r, 5)
    g = div(255 * g, 5)
    b = div(255 * b, 5)
    "rgb(#{r}, #{g}, #{b})"
  end

  defp color_to_css({:grayscale24, level}) do
    value = div(255 * level, 23)
    "rgb(#{value}, #{value}, #{value})"
  end
end
