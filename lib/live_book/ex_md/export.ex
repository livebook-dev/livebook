defmodule LiveBook.ExMd.Export do
  def notebook_to_markdown(notebook) do
    metadata = render_metadata(notebook.metadata)
    name = "# #{notebook.name}"

    sections =
      notebook.sections
      |> Enum.map(&render_section/1)
      |> Enum.join("\n\n")

    output = metadata <> "\n" <> name <> "\n\n" <> sections
    String.trim(output) <> "\n"
  end

  defp render_metadata(metadata) do
    metadata
    |> Enum.map(fn {key, value} ->
      # TODO: store and parse value as json?
      # TODO: what if the value has -->
      "<!--live_book:#{key}:#{value}-->"
    end)
    |> Enum.join("\n")
  end

  defp render_section(section) do
    metadata = render_metadata(section.metadata)
    name = "## #{section.name}"

    cells =
      section.cells
      |> Enum.map(&render_cell/1)
      |> Enum.join("\n\n")

    String.trim(metadata <> "\n" <> name <> "\n\n" <> cells)
  end

  defp render_cell(%{type: :markdown} = cell) do
    # TODO: reformat with Earmark parse -> markdown render (could actually do this for the whole noteobook)

    metadata = render_metadata(cell.metadata)
    md = cell.source

    String.trim(metadata <> "\n" <> md)
  end

  defp render_cell(%{type: :elixir} = cell) do
    metadata = render_metadata(cell.metadata)

    md = """
    ```elixir
    #{cell.source}
    ```
    """

    String.trim(metadata <> "\n" <> md)
  end
end
