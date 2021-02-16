defmodule LiveBook.LiveMarkdown.Export do
  alias LiveBook.{Notebook, Markdown}

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
    |> Markdown.reformat()
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
    metadata
    |> Enum.map(fn {key, value} ->
      value_json = Jason.encode!(value)
      "<!--live_book:#{key}:#{value_json}-->"
    end)
    |> Enum.intersperse("\n")
  end

  defp prepend_metadata(iodata, metadata) when metadata == %{}, do: iodata

  defp prepend_metadata(iodata, metadata) do
    content = render_metadata(metadata)
    [content, "\n", iodata]
  end
end
