defmodule Livebook.LiveMarkdown.MarkdownHelpers do
  @moduledoc false

  @doc """
  Wraps `EarmarkParser.as_ast/2`.
  """
  @spec markdown_to_ast(String.t()) :: {:ok | :error, EarmarkParser.ast(), list()}
  def markdown_to_ast(markdown) do
    EarmarkParser.as_ast(markdown)
  end

  @doc """
  Wraps `EarmarkParser.as_ast/2`.

  Markdown blocks are parsed into the AST, while inline
  content is kept as is.
  """
  @spec markdown_to_block_ast(String.t()) :: {:ok | :error, EarmarkParser.ast(), list()}
  def markdown_to_block_ast(markdown) do
    EarmarkParser.as_ast(markdown, parse_inline: false)
  end

  @doc """
  Reformats the given markdown document.
  """
  @spec reformat(String.t()) :: String.t()
  def reformat(markdown) do
    markdown
    |> markdown_to_block_ast()
    |> elem(1)
    |> markdown_from_ast()
  end

  @doc """
  Extracts plain text from the given AST ignoring all the tags.
  """
  @spec text_from_ast(EarmarkParser.ast()) :: String.t()
  def text_from_ast(ast)

  def text_from_ast(ast) when is_list(ast) do
    ast
    |> Enum.map(&text_from_ast/1)
    |> Enum.join("")
  end

  def text_from_ast(ast) when is_binary(ast), do: ast
  def text_from_ast({_, _, ast, _}), do: text_from_ast(ast)

  @doc """
  Determines suitable Markdown fence delimiter for the
  given code.
  """
  @spec code_block_delimiter(String.t()) :: String.t()
  def code_block_delimiter(code) do
    max_streak =
      Regex.scan(~r/`{3,}/, code)
      |> Enum.map(fn [string] -> byte_size(string) end)
      |> Enum.max(&>=/2, fn -> 2 end)

    String.duplicate("`", max_streak + 1)
  end

  @doc """
  Normalizes comments.

  In single-line comments trims all the whitespace and in multi-line
  comments removes trailing/leading blank newlines.
  """
  @spec normalize_comment_lines(list(String.t())) :: list(String.t())
  def normalize_comment_lines(lines)

  def normalize_comment_lines([line]) do
    [String.trim(line)]
  end

  def normalize_comment_lines(lines) do
    lines
    |> Enum.drop_while(&blank?/1)
    |> Enum.reverse()
    |> Enum.drop_while(&blank?/1)
    |> Enum.reverse()
  end

  @doc """
  Renders Markdown string from the given `EarmarkParser` AST.
  """
  @spec markdown_from_ast(EarmarkParser.ast()) :: String.t()
  def markdown_from_ast(ast) do
    build_md([], ast)
    |> IO.iodata_to_binary()
    |> String.trim()
  end

  defp build_md(iodata, ast)

  defp build_md(iodata, []), do: iodata

  defp build_md(iodata, [string | ast]) when is_binary(string) do
    string
    |> append_inline(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{tag, attrs, lines, %{verbatim: true}} | ast]) do
    render_html(tag, attrs, lines)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"em", _, content, %{}} | ast]) do
    render_emphasis(content)
    |> append_inline(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"strong", _, content, %{}} | ast]) do
    render_strong(content)
    |> append_inline(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"del", _, content, %{}} | ast]) do
    render_strikethrough(content)
    |> append_inline(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"code", _, content, %{}} | ast]) do
    render_code_inline(content)
    |> append_inline(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"a", attrs, content, %{}} | ast]) do
    render_link(content, attrs)
    |> append_inline(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"img", attrs, [], %{}} | ast]) do
    render_image(attrs)
    |> append_inline(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{:comment, _, lines, %{comment: true}} | ast]) do
    render_comment(lines)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"hr", attrs, [], %{}} | ast]) do
    render_ruler(attrs)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"br", _, [], %{}} | ast]) do
    render_line_break()
    |> append_inline(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"p", _, content, %{}} | ast]) do
    render_paragraph(content)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"h" <> n, _, content, %{}} | ast])
       when n in ["1", "2", "3", "4", "5", "6"] do
    n = String.to_integer(n)

    render_heading(n, content)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"pre", _, [{"code", attrs, [content], %{}}], %{}} | ast]) do
    render_code_block(content, attrs)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"blockquote", [], content, %{}} | ast]) do
    render_blockquote(content)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"table", _, content, %{}} | ast]) do
    render_table(content)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"ul", _, content, %{}} | ast]) do
    render_unordered_list(content)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{"ol", _, content, %{}} | ast]) do
    render_ordered_list(content)
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp append_inline(md, iodata), do: [iodata, md]
  defp append_block(md, iodata), do: [iodata, "\n", md, "\n"]

  # Renderers

  # https://www.w3.org/TR/2011/WD-html-markup-20110113/syntax.html#void-element
  @void_elements ~W(area base br col command embed hr img input keygen link meta param source track wbr)

  defp render_html(tag, attrs, []) when tag in @void_elements do
    ["<", tag, attrs_to_string(attrs), " />"]
  end

  defp render_html(tag, attrs, lines) do
    inner = Enum.intersperse(lines, "\n")
    ["<", tag, attrs_to_string(attrs), ">\n", inner, "\n</", tag, ">"]
  end

  defp render_emphasis(content) do
    inner = markdown_from_ast(content)
    ["*", inner, "*"]
  end

  defp render_strong(content) do
    inner = markdown_from_ast(content)
    ["**", inner, "**"]
  end

  defp render_strikethrough(content) do
    inner = markdown_from_ast(content)
    ["~~", inner, "~~"]
  end

  defp render_code_inline(content) do
    inner = markdown_from_ast(content)
    ["`", inner, "`"]
  end

  defp render_link(content, attrs) do
    caption = build_md([], content)
    href = get_attr(attrs, "href", "")
    ["[", caption, "](", href, ")"]
  end

  defp render_image(attrs) do
    alt = get_attr(attrs, "alt", "")
    src = get_attr(attrs, "src", "")
    title = get_attr(attrs, "title", "")

    if title == "" do
      ["![", alt, "](", src, ")"]
    else
      ["![", alt, "](", src, ~s/ "/, title, ~s/")/]
    end
  end

  defp render_comment(lines) do
    case normalize_comment_lines(lines) do
      [line] -> ["<!-- ", line, " -->"]
      lines -> ["<!--\n", Enum.intersperse(lines, "\n"), "\n-->"]
    end
  end

  defp render_ruler(attrs) do
    class = get_attr(attrs, "class", "thin")

    case class do
      "thin" -> "---"
      "medium" -> "___"
      "thick" -> "***"
    end
  end

  defp render_line_break(), do: "\\\n"

  defp render_paragraph(content), do: markdown_from_ast(content)

  defp render_heading(n, content) do
    title = build_md([], content)
    [String.duplicate("#", n), " ", title]
  end

  defp render_code_block(content, attrs) do
    delimiter = code_block_delimiter(content)
    language = get_attr(attrs, "class", "")
    [delimiter, language, "\n", content, "\n", delimiter]
  end

  defp render_blockquote(content) do
    inner = markdown_from_ast(content)

    inner
    |> String.split("\n")
    |> Enum.map_intersperse("\n", &["> ", &1])
  end

  defp render_table([{"thead", _, [head_row], %{}}, {"tbody", _, body_rows, %{}}]) do
    alignments = alignments_from_row(head_row)
    cell_grid = cell_grid_from_rows([head_row | body_rows])
    column_widths = max_length_per_column(cell_grid)
    [head_cells | body_cell_grid] = Enum.map(cell_grid, &pad_whitespace(&1, column_widths))
    separator_cells = build_separator_cells(alignments, column_widths)
    cell_grid_to_md_table([head_cells, separator_cells | body_cell_grid])
  end

  defp render_table([{"tbody", _, body_rows, %{}}]) do
    cell_grid = cell_grid_from_rows(body_rows)
    column_widths = max_length_per_column(cell_grid)
    cell_grid = Enum.map(cell_grid, &pad_whitespace(&1, column_widths))
    cell_grid_to_md_table(cell_grid)
  end

  defp cell_grid_from_rows(rows) do
    Enum.map(rows, fn {"tr", _, columns, %{}} ->
      Enum.map(columns, fn {tag, _, content, %{}} when tag in ["th", "td"] ->
        build_md([], content)
      end)
    end)
  end

  defp alignments_from_row({"tr", _, columns, %{}}) do
    Enum.map(columns, fn {tag, attrs, _, %{}} when tag in ["th", "td"] ->
      style = get_attr(attrs, "style", nil)

      case style do
        "text-align: left;" -> :left
        "text-align: center;" -> :center
        "text-align: right;" -> :right
      end
    end)
  end

  defp build_separator_cells(alignments, widths) do
    alignments
    |> Enum.zip(widths)
    |> Enum.map(fn
      {:left, length} -> String.duplicate("-", length)
      {:center, length} -> ":" <> String.duplicate("-", length - 2) <> ":"
      {:right, length} -> String.duplicate("-", length - 1) <> ":"
    end)
  end

  defp max_length_per_column(cell_grid) do
    cell_grid
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.map(fn cells ->
      cells
      |> Enum.map(&IO.iodata_length/1)
      |> Enum.max()
    end)
  end

  defp pad_whitespace(cells, widths) do
    cells
    |> Enum.zip(widths)
    |> Enum.map(fn {cell, width} ->
      length = IO.iodata_length(cell)
      missing = max(width - length, 0)
      [cell, String.duplicate(" ", missing)]
    end)
  end

  defp cell_grid_to_md_table(cell_grid) do
    cell_grid
    |> Enum.map(fn cells ->
      ["| ", Enum.intersperse(cells, " | "), " |"]
    end)
    |> Enum.intersperse("\n")
  end

  defp render_unordered_list(content) do
    marker_fun = fn _index -> "* " end
    render_list(content, marker_fun, "  ")
  end

  defp render_ordered_list(content) do
    marker_fun = fn index -> "#{index + 1}. " end
    render_list(content, marker_fun, "   ")
  end

  defp render_list(items, marker_fun, indent) do
    spaced? = spaced_list_items?(items)
    item_separator = if(spaced?, do: "\n\n", else: "\n")

    items
    |> Enum.map(fn {"li", _, content, %{}} -> markdown_from_ast(content) end)
    |> Enum.with_index()
    |> Enum.map(fn {inner, index} ->
      [first_line | lines] = String.split(inner, "\n")

      first_line = [marker_fun.(index), first_line]

      lines =
        Enum.map(lines, fn
          "" -> ""
          line -> [indent, line]
        end)

      Enum.intersperse([first_line | lines], "\n")
    end)
    |> Enum.intersperse(item_separator)
  end

  defp spaced_list_items?([{"li", _, [{"p", _, _content, %{}} | _], %{}} | _items]), do: true
  defp spaced_list_items?([_ | items]), do: spaced_list_items?(items)
  defp spaced_list_items?([]), do: false

  # Helpers

  defp get_attr(attrs, key, default) do
    Enum.find_value(attrs, default, fn {attr_key, attr_value} ->
      attr_key == key && attr_value
    end)
  end

  defp attrs_to_string(attrs) do
    Enum.map(attrs, fn {key, value} -> ~s/ #{key}="#{value}"/ end)
  end

  defp blank?(string), do: String.trim(string) == ""
end
