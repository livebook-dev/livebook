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
    include_outputs? = Keyword.get(opts, :include_outputs, notebook.persist_outputs)

    js_ref_with_data = if include_outputs?, do: collect_js_output_data(notebook), else: %{}

    ctx = %{include_outputs?: include_outputs?, js_ref_with_data: js_ref_with_data}

    iodata = render_notebook(notebook, ctx)
    # Add trailing newline
    IO.iodata_to_binary([iodata, "\n"])
  end

  defp collect_js_output_data(notebook) do
    for section <- notebook.sections,
        %Cell.Elixir{} = cell <- section.cells,
        {_idx, {:js, %{export: %{}, ref: ref, pid: pid}}} <- cell.outputs do
      Task.async(fn ->
        {ref, get_js_output_data(pid, ref)}
      end)
    end
    |> Task.await_many(:infinity)
    |> Map.new()
  end

  defp get_js_output_data(pid, ref) do
    send(pid, {:connect, self(), %{origin: self(), ref: ref}})

    monitor_ref = Process.monitor(pid)

    data =
      receive do
        {:connect_reply, data, %{ref: ^ref}} -> data
        {:DOWN, ^monitor_ref, :process, _pid, _reason} -> nil
      end

    Process.demonitor(monitor_ref, [:flush])

    data
  end

  defp render_notebook(notebook, ctx) do
    comments =
      Enum.map(notebook.leading_comments, fn
        [line] -> ["<!-- ", line, " -->"]
        lines -> ["<!--\n", Enum.intersperse(lines, "\n"), "\n-->"]
      end)

    name = ["# ", notebook.name]
    sections = Enum.map(notebook.sections, &render_section(&1, notebook, ctx))

    metadata = notebook_metadata(notebook)

    notebook_with_metadata =
      [name | sections]
      |> Enum.intersperse("\n\n")
      |> prepend_metadata(metadata)

    Enum.intersperse(comments ++ [notebook_with_metadata], "\n\n")
  end

  defp notebook_metadata(notebook) do
    put_unless_default(
      %{},
      Map.take(notebook, [:persist_outputs, :autosave_interval_s]),
      Map.take(Notebook.new(), [:persist_outputs, :autosave_interval_s])
    )
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
    outputs = if ctx.include_outputs?, do: render_outputs(cell, ctx), else: []

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

  defp cell_metadata(%Cell.Elixir{} = cell) do
    put_unless_default(
      %{},
      Map.take(cell, [:disable_formatting, :reevaluate_automatically]),
      Map.take(Cell.Elixir.new(), [:disable_formatting, :reevaluate_automatically])
    )
  end

  defp cell_metadata(_cell), do: %{}

  defp render_outputs(cell, ctx) do
    cell.outputs
    |> Enum.reverse()
    |> Enum.map(fn {_idx, output} -> render_output(output, ctx) end)
    |> Enum.reject(&(&1 == :ignored))
    |> Enum.intersperse("\n\n")
  end

  defp render_output({:stdout, text}, _ctx) do
    text = String.replace_suffix(text, "\n", "")
    delimiter = MarkdownHelpers.code_block_delimiter(text)
    text = strip_ansi(text)
    [delimiter, "output\n", text, "\n", delimiter]
  end

  defp render_output({:text, text}, _ctx) do
    delimiter = MarkdownHelpers.code_block_delimiter(text)
    text = strip_ansi(text)
    [delimiter, "output\n", text, "\n", delimiter]
  end

  defp render_output({:vega_lite_static, spec}, _ctx) do
    ["```", "vega-lite\n", Jason.encode!(spec), "\n", "```"]
  end

  defp render_output(
         {:js, %{export: %{info_string: info_string, key: key}, ref: ref}},
         ctx
       )
       when is_binary(info_string) do
    data = ctx.js_ref_with_data[ref]
    payload = if key && is_map(data), do: data[key], else: data

    case encode_js_data(payload) do
      {:ok, binary} ->
        ["```", info_string, "\n", binary, "\n", "```"]

      _ ->
        :ignored
    end
  end

  defp render_output(_output, _ctx), do: :ignored

  defp encode_js_data(data) when is_binary(data), do: {:ok, data}
  defp encode_js_data(data), do: Jason.encode(data)

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

  defp put_unless_default(map, entries, defaults) do
    Enum.reduce(entries, map, fn {key, value}, map ->
      if value == defaults[key] do
        map
      else
        Map.put(map, key, value)
      end
    end)
  end

  defp strip_ansi(string) do
    string
    |> Livebook.Utils.ANSI.parse_ansi_string()
    |> elem(0)
    |> Enum.map(fn {_modifiers, string} -> string end)
  end
end
