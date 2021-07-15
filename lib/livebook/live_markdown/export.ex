defmodule Livebook.LiveMarkdown.Export do
  alias Livebook.Notebook
  alias Livebook.Notebook.Cell
  alias Livebook.LiveMarkdown.MarkdownHelpers

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
    sections = Enum.map(notebook.sections, &render_section(&1, notebook))

    [name | sections]
    |> Enum.intersperse("\n\n")
    |> prepend_metadata(notebook.metadata)
  end

  defp render_section(section, notebook) do
    name = "## #{section.name}"
    cells = Enum.map(section.cells, &render_cell/1)
    metadata = section_metadata(section, notebook)

    [name | cells]
    |> Enum.intersperse("\n\n")
    |> prepend_metadata(metadata)
  end

  defp section_metadata(%{parent_id: nil} = section, _notebook) do
    section.metadata
  end

  defp section_metadata(section, notebook) do
    parent_idx = Notebook.section_index(notebook, section.parent_id)
    Map.put(section.metadata, "branch_parent_index", parent_idx)
  end

  defp render_cell(%Cell.Markdown{} = cell) do
    cell.source
    |> format_markdown_source()
    |> prepend_metadata(cell.metadata)
  end

  defp render_cell(%Cell.Elixir{} = cell) do
    code = get_elixir_cell_code(cell)
    delimiter = code_block_delimiter(code)

    """
    #{delimiter}elixir
    #{code}
    #{delimiter}\
    """
    |> prepend_metadata(cell.metadata)
  end

  defp render_cell(%Cell.Input{} = cell) do
    value = if cell.type == :password, do: "", else: cell.value

    json =
      %{
        livebook_object: :cell_input,
        type: cell.type,
        name: cell.name,
        value: value
      }
      |> put_unless_implicit(reactive: cell.reactive, props: cell.props)
      |> Jason.encode!()

    "<!-- livebook:#{json} -->"
    |> prepend_metadata(cell.metadata)
  end

  defp get_elixir_cell_code(%{source: source, metadata: %{"disable_formatting" => true}}),
    do: source

  defp get_elixir_cell_code(%{source: source}), do: format_code(source)

  defp render_metadata(metadata) do
    metadata_json = Jason.encode!(metadata)
    "<!-- livebook:#{metadata_json} -->"
  end

  defp prepend_metadata(iodata, metadata) when metadata == %{}, do: iodata

  defp prepend_metadata(iodata, metadata) do
    content = render_metadata(metadata)
    [content, "\n\n", iodata]
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
    |> add_markdown_annotation_before_elixir_block()
  end

  defp remove_reserved_headings(ast) do
    Enum.filter(ast, fn
      {"h1", _, _, _} -> false
      {"h2", _, _, _} -> false
      _ast_node -> true
    end)
  end

  defp add_markdown_annotation_before_elixir_block(ast) do
    Enum.flat_map(ast, fn
      {"pre", _, [{"code", [{"class", "elixir"}], [_source], %{}}], %{}} = ast_node ->
        [{:comment, [], [~s/livebook:{"force_markdown":true}/], %{comment: true}}, ast_node]

      ast_node ->
        [ast_node]
    end)
  end

  defp format_code(code) do
    try do
      code
      |> Code.format_string!()
      |> IO.iodata_to_binary()
    rescue
      _ -> code
    end
  end

  defp code_block_delimiter(code) do
    max_streak =
      Regex.scan(~r/`{3,}/, code)
      |> Enum.map(fn [string] -> byte_size(string) end)
      |> Enum.max(&>=/2, fn -> 2 end)

    String.duplicate("`", max_streak + 1)
  end

  defp put_unless_implicit(map, entries) do
    Enum.reduce(entries, map, fn {key, value}, map ->
      if value in [false, %{}] do
        map
      else
        Map.put(map, key, value)
      end
    end)
  end
end
