defmodule LiveBook.Evaluator.StringFormatter do
  @moduledoc false

  # The formatter used by LiveBook for rendering the results.

  @behaviour LiveBook.Evaluator.Formatter

  @impl true
  def format({:ok, value}) do
    inspected = inspect_as_html(value, pretty: true, width: 100)
    {:inspect_html, inspected}
  end

  def format({:error, kind, error, stacktrace}) do
    formatted = Exception.format(kind, error, stacktrace)
    {:error, formatted}
  end

  @doc """
  Wraps `inspect/2` to include HTML tags in the final string for syntax highlighting.

  Any options given as the second argument are passed directly to `inspect/2`.

  ## Examples

      iex> StringFormatter.inspect_as_html(:test, [])
      "<span class=\\"atom\\">:test</span>"
  """
  @spec inspect_as_html(Inspect.t(), keyword()) :: String.t()
  def inspect_as_html(term, opts \\ []) do
    # Inspect coloring primary tragets terminals,
    # so the colors are usually ANSI escape codes
    # and their effect is reverted by a special reset escape code.
    #
    # In our case we need HTML tags for syntax highlighting,
    # so as the colors we use sequences like \xfeatom\xfe
    # then we HTML-escape the string and finally replace
    # these special sequences with actual <span> tags.
    #
    # Note that the surrounding \xfe byte is invalid in a UTF-8 sequence,
    # so we can be certain it won't appear in the normal `inspect` result.

    term
    |> inspect(Keyword.merge(opts, syntax_colors: inspect_html_colors()))
    |> html_escape()
    |> replace_colors_with_tags()
  end

  defp inspect_html_colors() do
    delim = "\xfe"

    [
      atom: delim <> "atom" <> delim,
      binary: delim <> "binary" <> delim,
      boolean: delim <> "boolean" <> delim,
      list: delim <> "list" <> delim,
      map: delim <> "map" <> delim,
      number: delim <> "number" <> delim,
      nil: delim <> "nil" <> delim,
      regex: delim <> "regex" <> delim,
      string: delim <> "string" <> delim,
      tuple: delim <> "tuple" <> delim,
      reset: delim <> "reset" <> delim
    ]
  end

  defp replace_colors_with_tags(string) do
    colors = inspect_html_colors()

    Enum.reduce(colors, string, fn
      {:reset, color}, string ->
        String.replace(string, color, "</span>")

      {key, color}, string ->
        String.replace(string, color, "<span class=\"#{Atom.to_string(key)}\">")
    end)
  end

  # Escapes the given HTML to string.
  # Taken from https://github.com/elixir-plug/plug/blob/692655393a090fbae544f5cd10255d4d600e7bb0/lib/plug/html.ex#L37
  defp html_escape(data) when is_binary(data) do
    IO.iodata_to_binary(to_iodata(data, 0, data, []))
  end

  escapes = [
    {?<, "&lt;"},
    {?>, "&gt;"},
    {?&, "&amp;"},
    {?", "&quot;"},
    {?', "&#39;"}
  ]

  for {match, insert} <- escapes do
    defp to_iodata(<<unquote(match), rest::bits>>, skip, original, acc) do
      to_iodata(rest, skip + 1, original, [acc | unquote(insert)])
    end
  end

  defp to_iodata(<<_char, rest::bits>>, skip, original, acc) do
    to_iodata(rest, skip, original, acc, 1)
  end

  defp to_iodata(<<>>, _skip, _original, acc) do
    acc
  end

  for {match, insert} <- escapes do
    defp to_iodata(<<unquote(match), rest::bits>>, skip, original, acc, len) do
      part = binary_part(original, skip, len)
      to_iodata(rest, skip + len + 1, original, [acc, part | unquote(insert)])
    end
  end

  defp to_iodata(<<_char, rest::bits>>, skip, original, acc, len) do
    to_iodata(rest, skip, original, acc, len + 1)
  end

  defp to_iodata(<<>>, 0, original, _acc, _len) do
    original
  end

  defp to_iodata(<<>>, skip, original, acc, len) do
    [acc | binary_part(original, skip, len)]
  end
end
