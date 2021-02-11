defmodule LiveBookWeb.Helpers do
  import Phoenix.LiveView.Helpers

  @doc """
  Wraps `inspect/2` to include HTML tags in the final string for syntax highlighting.

  Any options given as the second argument are passed directly to `inspect/2`.

  ## Examples

      iex(2)> LiveBookWeb.Helpers.inspect_as_html(:test, [])
      {:safe, "<span class=\\"atom\\">:test</span>"}
  """
  @spec inspect_as_html(Inspect.t(), keyword()) :: Phoenix.HTML.safe()
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
    |> Phoenix.HTML.html_escape()
    |> Phoenix.HTML.safe_to_string()
    |> replace_colors_with_tags()
    |> Phoenix.HTML.raw()
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

  @doc """
  Renders a component inside the `LiveBook.ModalComponent` component.

  The rendered modal receives a `:return_to` option to properly update
  the URL when the modal is closed.
  """
  def live_modal(socket, component, opts) do
    path = Keyword.fetch!(opts, :return_to)
    modal_opts = [id: :modal, return_to: path, component: component, opts: opts]
    live_component(socket, LiveBookWeb.ModalComponent, modal_opts)
  end
end
