defmodule Livebook.LiveMarkdown.Export do
  alias Livebook.Notebook
  alias Livebook.Notebook.Cell
  alias Livebook.LiveMarkdown.MarkdownHelpers

  @doc """
  Converts the given notebook into a Markdown document.

  ## Options

    * `:include_outputs` - whether to render cell outputs.
      Only textual outputs are included. Defaults to the
      value of `:persist_outputs` notebook attribute.
  """
  @spec notebook_to_markdown(Notebook.t(), keyword()) :: String.t()
  def notebook_to_markdown(notebook, opts \\ []) do
    ctx = %{
      include_outputs?: Keyword.get(opts, :include_outputs, notebook.persist_outputs)
    }

    iodata = render_notebook(notebook, ctx)
    # Add trailing newline
    IO.iodata_to_binary([iodata, "\n"])
  end

  defp render_notebook(notebook, ctx) do
    name = ["# ", notebook.name]
    sections = Enum.map(notebook.sections, &render_section(&1, notebook, ctx))

    metadata = notebook_metadata(notebook)

    [name | sections]
    |> Enum.intersperse("\n\n")
    |> prepend_metadata(metadata)
  end

  defp notebook_metadata(notebook) do
    put_unless_implicit(%{}, persist_outputs: notebook.persist_outputs)
  end

  defp render_section(section, notebook, ctx) do
    name = ["## ", section.name]

    {cells, _} =
      Enum.map_reduce(section.cells, nil, fn cell, prev_cell ->
        separator =
          if is_struct(cell, Cell.Markdown) and is_struct(prev_cell, Cell.Markdown) do
            [~s/<!-- livebook:{"break_markdown":true} -->\n\n/]
          else
            []
          end

        rendered = separator ++ [render_cell(cell, ctx)]
        {rendered, cell}
      end)

    metadata = section_metadata(section, notebook)

    [name | cells]
    |> Enum.intersperse("\n\n")
    |> prepend_metadata(metadata)
  end

  defp section_metadata(%{parent_id: nil} = _section, _notebook) do
    %{}
  end

  defp section_metadata(section, notebook) do
    parent_idx = Notebook.section_index(notebook, section.parent_id)
    %{"branch_parent_index" => parent_idx}
  end

  defp render_cell(%Cell.Markdown{} = cell, _ctx) do
    metadata = cell_metadata(cell)

    cell.source
    |> format_markdown_source()
    |> prepend_metadata(metadata)
  end

  defp render_cell(%Cell.Elixir{} = cell, ctx) do
    delimiter = MarkdownHelpers.code_block_delimiter(cell.source)
    code = get_elixir_cell_code(cell)
    outputs = if ctx.include_outputs?, do: render_outputs(cell), else: []

    metadata = cell_metadata(cell)

    cell =
      [delimiter, "elixir\n", code, "\n", delimiter]
      |> prepend_metadata(metadata)

    if outputs == [] do
      cell
    else
      [cell, "\n\n", outputs]
    end
  end

  defp render_cell(%Cell.Input{} = cell, _ctx) do
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

    metadata = cell_metadata(cell)

    "<!-- livebook:#{json} -->"
    |> prepend_metadata(metadata)
  end

  defp cell_metadata(%Cell.Elixir{} = cell) do
    put_unless_implicit(%{}, disable_formatting: cell.disable_formatting)
  end

  defp cell_metadata(_cell), do: %{}

  defp render_outputs(cell) do
    cell.outputs
    |> Enum.reverse()
    |> Enum.map(&render_output/1)
    |> Enum.reject(&(&1 == :ignored))
    |> Enum.intersperse("\n\n")
  end

  defp render_output(text) when is_binary(text) do
    text = String.replace_suffix(text, "\n", "")
    delimiter = MarkdownHelpers.code_block_delimiter(text)
    text = strip_ansi(text)
    [delimiter, "output\n", text, "\n", delimiter]
  end

  defp render_output({:text, text}) do
    delimiter = MarkdownHelpers.code_block_delimiter(text)
    text = strip_ansi(text)
    [delimiter, "output\n", text, "\n", delimiter]
  end

  defp render_output(_output), do: :ignored

  defp get_elixir_cell_code(%{source: source, disable_formatting: true}),
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
    |> MarkdownHelpers.markdown_to_block_ast()
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
      Code.format_string!(code)
    rescue
      _ -> code
    end
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

  defp strip_ansi(string) do
    string
    |> Livebook.Utils.ANSI.parse_ansi_string()
    |> Enum.map(fn {_modifiers, string} -> string end)
  end
end
