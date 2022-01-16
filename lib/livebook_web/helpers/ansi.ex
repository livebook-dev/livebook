defmodule LivebookWeb.Helpers.ANSI do
  @moduledoc false

  @doc """
  Takes a string with ANSI escape codes and build a HTML safe string
  with `span` tags having classes corresponding to the escape codes.

  Any HTML in the string is escaped.
  """
  @spec ansi_string_to_html(String.t()) :: Phoenix.HTML.safe()
  def ansi_string_to_html(string) do
    string
    |> Livebook.Utils.ANSI.parse_ansi_string()
    |> elem(0)
    |> parts_to_html()
  end

  defp parts_to_html(parts) do
    parts
    |> Enum.map(fn {modifiers, string} ->
      style = modifiers_to_css(modifiers)
      {:safe, escaped} = Phoenix.HTML.html_escape(string)

      if style == "" or string == "" do
        escaped
      else
        [~s{<span style="}, style, ~s{">}, escaped, ~s{</span>}]
      end
    end)
    |> Phoenix.HTML.raw()
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

  @doc """
  Converts a string with ANSI escape codes into HTML lines.

  This method is similar to `ansi_string_to_html/1`,
  but makes sure each line is itself a valid HTML
  (as opposed to just splitting HTML into lines).
  """
  @spec ansi_string_to_html_lines(String.t()) :: list(Phoenix.HTML.safe())
  def ansi_string_to_html_lines(string) do
    string
    |> Livebook.Utils.ANSI.parse_ansi_string()
    |> elem(0)
    |> split_parts_into_lines()
    |> Enum.map(&parts_to_html/1)
  end

  defp split_parts_into_lines(parts), do: split_parts_into_lines(parts, [[]])

  defp split_parts_into_lines([], groups) do
    groups
    |> Enum.map(&Enum.reverse/1)
    |> Enum.reverse()
  end

  defp split_parts_into_lines([{modifiers, string} | parts], [group | groups]) do
    [line | lines] = String.split(string, "\n")
    new_groups = lines |> Enum.map(&[{modifiers, &1}]) |> Enum.reverse()
    split_parts_into_lines(parts, new_groups ++ [[{modifiers, line} | group] | groups])
  end

  @doc """
  Converts a string with ANSI escape codes into HTML lines.

  Same as `ansi_string_to_html_lines_step/1`, but allows
  for keeping track of modifiers for stream usage.
  """
  @spec ansi_string_to_html_lines_step(String.t(), Livebook.Utils.ANSI.modifiers()) ::
          {list(Phoenix.HTML.safe()), Livebook.Utils.ANSI.modifiers()}
  def ansi_string_to_html_lines_step(string, modifiers) do
    {parts, modifiers} = Livebook.Utils.ANSI.parse_ansi_string(string, modifiers: modifiers)

    lines =
      parts
      |> split_parts_into_lines()
      |> Enum.map(&parts_to_html/1)

    {lines, modifiers}
  end
end
