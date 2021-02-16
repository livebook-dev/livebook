defmodule LiveBook.LiveMarkdown.Export do
  alias LiveBook.Notebook
  alias LiveBook.LiveMarkdown.MarkdownHelpers

  @doc """
  Converts the given notebook into a Markdown document.
  """
  @spec notebook_to_markdown(Notebook.t()) :: String.t()
  def notebook_to_markdown(notebook) do
    iodata = render_notebook(notebook)
    # Add trailing newline
    IO.iodata_to_binary([iodata, "\n"])
  end

  defp render_notebook(notebook) do
    name = "# #{notebook.name}"
    sections = Enum.map(notebook.sections, &render_section/1)

    [name | sections]
    |> Enum.intersperse("\n\n")
    |> prepend_metadata(notebook.metadata)
  end

  defp render_section(section) do
    name = "## #{section.name}"
    cells = Enum.map(section.cells, &render_cell/1)

    [name | cells]
    |> Enum.intersperse("\n\n")
    |> prepend_metadata(section.metadata)
  end

  defp render_cell(%{type: :markdown} = cell) do
    cell.source
    |> format_markdown_source()
    |> prepend_metadata(cell.metadata)
  end

  defp render_cell(%{type: :elixir} = cell) do
    """
    ```elixir
    #{cell.source}
    ```\
    """
    |> prepend_metadata(cell.metadata)
  end

  defp render_metadata(metadata) do
    metadata_json = Jason.encode!(metadata)
    "<!--live_book:#{metadata_json}-->"
  end

  defp prepend_metadata(iodata, metadata) when metadata == %{}, do: iodata

  defp prepend_metadata(iodata, metadata) do
    content = render_metadata(metadata)
    [content, "\n", iodata]
  end

  defp format_markdown_source(markdown) do
    markdown
    |> EarmarkParser.as_ast()
    |> elem(1)
    |> rewrite_ast()
    |> MarkdownHelpers.markdown_from_ast()
  end

  # Alters AST of the user-entered markdown.
  defp rewrite_ast(ast) do
    ast
    |> remove_reserved_headings()
  end

  defp remove_reserved_headings(ast) do
    Enum.filter(ast, fn
      {"h1", _, _, _} -> false
      {"h2", _, _, _} -> false
      _ast_node -> true
    end)
  end
end
