defmodule Livebook.LiveMarkdown.Export do
  alias Livebook.Notebook
  alias Livebook.Notebook.Cell
  alias Livebook.LiveMarkdown.MarkdownHelpers

  def notebook_to_livemd(notebook, opts \\ []) do
    include_outputs? = Keyword.get(opts, :include_outputs, notebook.persist_outputs)
    include_stamp? = Keyword.get(opts, :include_stamp, true)

    js_ref_with_export = if include_outputs?, do: collect_js_output_export(notebook), else: %{}

    ctx = %{include_outputs?: include_outputs?, js_ref_with_export: js_ref_with_export}

    iodata = render_notebook(notebook, ctx)

    # Add trailing newline
    notebook_source = [iodata, "\n"]

    {notebook_footer, footer_warnings} =
      render_notebook_footer(notebook, notebook_source, include_stamp?)

    source = IO.iodata_to_binary([notebook_source, notebook_footer])

    {source, footer_warnings}
  end

  defp collect_js_output_export(notebook) do
    for(
      section <- notebook.sections,
      %{outputs: outputs} <- section.cells,
      {_idx, %{type: :js, js_view: js_view, export: true}} <- outputs,
      do: {js_view.ref, js_view.pid},
      uniq: true
    )
    |> Enum.map(fn {ref, pid} ->
      Task.async(fn -> {ref, get_js_output_export(pid, ref)} end)
    end)
    |> Task.await_many(:infinity)
    |> Map.new()
  end

  defp get_js_output_export(pid, ref) do
    send(pid, {:export, self(), %{ref: ref}})

    monitor_ref = Process.monitor(pid)

    data =
      receive do
        {:export_reply, export_result, %{ref: ^ref}} -> export_result
        {:DOWN, ^monitor_ref, :process, _pid, _reason} -> nil
      end

    Process.demonitor(monitor_ref, [:flush])

    data
  end

  defp render_notebook(notebook, ctx) do
    %{setup_section: %{cells: [setup_cell]}} = notebook

    comments =
      Enum.map(notebook.leading_comments, fn
        [line] -> ["<!-- ", line, " -->"]
        lines -> ["<!--\n", Enum.intersperse(lines, "\n"), "\n-->"]
      end)

    name = ["# ", notebook.name]
    setup_cell = render_setup_cell(setup_cell, %{ctx | include_outputs?: false})
    sections = Enum.map(notebook.sections, &render_section(&1, notebook, ctx))

    metadata = notebook_metadata(notebook)

    notebook_with_metadata =
      [name, setup_cell | sections]
      |> Enum.reject(&is_nil/1)
      |> Enum.intersperse("\n\n")
      |> prepend_metadata(metadata)

    Enum.intersperse(comments ++ [notebook_with_metadata], "\n\n")
  end

  defp notebook_metadata(notebook) do
    keys = [
      :persist_outputs,
      :autosave_interval_s,
      :default_language,
      :hub_id,
      :deployment_group_id
    ]

    metadata = put_unless_default(%{}, Map.take(notebook, keys), Map.take(Notebook.new(), keys))

    app_settings_metadata = app_settings_metadata(notebook.app_settings)

    file_entry_metadata =
      notebook.file_entries
      |> Enum.sort_by(& &1.name)
      |> Enum.map(&file_entry_metadata/1)

    put_unless_default(
      metadata,
      %{app_settings: app_settings_metadata, file_entries: file_entry_metadata},
      %{app_settings: %{}, file_entries: []}
    )
  end

  defp app_settings_metadata(app_settings) do
    keys = [
      :slug,
      :multi_session,
      :zero_downtime,
      :show_existing_sessions,
      :auto_shutdown_ms,
      :access_type,
      :show_source,
      :output_type
    ]

    put_unless_default(
      %{},
      Map.take(app_settings, keys),
      Map.take(Notebook.AppSettings.new(), keys)
    )
  end

  defp file_entry_metadata(%{type: :attachment, name: name}) do
    %{type: "attachment", name: name}
  end

  defp file_entry_metadata(%{type: :file, name: name, file: file}) do
    %{
      type: "file",
      name: name,
      file: %{
        file_system_id: file.file_system_id,
        file_system_type: Livebook.FileSystems.module_to_type(file.file_system_module),
        path: file.path
      }
    }
  end

  defp file_entry_metadata(%{type: :url, name: name, url: url}) do
    %{type: "url", name: name, url: url}
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

  defp render_setup_cell(%{source: ""}, _ctx), do: nil
  defp render_setup_cell(cell, ctx), do: render_cell(cell, ctx)

  defp render_cell(%Cell.Markdown{} = cell, _ctx) do
    metadata = cell_metadata(cell)

    cell.source
    |> format_markdown_source()
    |> prepend_metadata(metadata)
  end

  defp render_cell(%Cell.Code{} = cell, ctx) do
    delimiter = MarkdownHelpers.code_block_delimiter(cell.source)
    code = cell.source
    outputs = if ctx.include_outputs?, do: render_outputs(cell, ctx), else: []

    metadata = cell_metadata(cell)

    cell =
      [delimiter, Atom.to_string(cell.language), "\n", code, "\n", delimiter]
      |> prepend_metadata(metadata)

    if outputs == [] do
      cell
    else
      [cell, "\n\n", outputs]
    end
  end

  defp render_cell(%Cell.Smart{} = cell, ctx) do
    %{Cell.Code.new() | source: cell.source, outputs: cell.outputs}
    |> render_cell(ctx)
    |> prepend_metadata(%{
      "livebook_object" => "smart_cell",
      "kind" => cell.kind,
      # Attributes may include arbitrary values, including sequences
      # like "-->" that would mess our format, so we always encode them
      "attrs" => cell.attrs |> ensure_order() |> Jason.encode!() |> Base.encode64(padding: false),
      "chunks" => cell.chunks && Enum.map(cell.chunks, &Tuple.to_list/1)
    })
  end

  defp cell_metadata(%Cell.Code{} = cell) do
    keys = [:reevaluate_automatically, :continue_on_error]
    put_unless_default(%{}, Map.take(cell, keys), Map.take(Cell.Code.new(), keys))
  end

  defp cell_metadata(_cell), do: %{}

  defp render_outputs(cell, ctx) do
    cell.outputs
    |> Enum.reverse()
    |> Enum.map(fn {_idx, output} -> render_output(output, ctx) end)
    |> Enum.reject(&(&1 == :ignored))
    |> Enum.intersperse("\n\n")
  end

  defp render_output(%{type: :terminal_text, text: text}, _ctx) do
    text = String.replace_suffix(text, "\n", "")
    delimiter = MarkdownHelpers.code_block_delimiter(text)
    text = strip_ansi(text)

    [delimiter, "\n", text, "\n", delimiter]
    |> prepend_metadata(%{output: true})
  end

  defp render_output(%{type: :js, js_view: %{ref: ref}}, ctx) do
    with {info_string, payload} <- ctx.js_ref_with_export[ref],
         {:ok, binary} <- encode_js_data(payload) do
      delimiter = MarkdownHelpers.code_block_delimiter(binary)

      [delimiter, info_string, "\n", binary, "\n", delimiter]
      |> prepend_metadata(%{output: true})
    else
      _ -> :ignored
    end
  end

  defp render_output(%{type: :tabs, outputs: outputs}, ctx) do
    Enum.find_value(outputs, :ignored, fn {_idx, output} ->
      case render_output(output, ctx) do
        :ignored -> nil
        rendered -> rendered
      end
    end)
  end

  defp render_output(%{type: :grid, outputs: outputs}, ctx) do
    outputs
    |> Enum.map(fn {_idx, output} -> render_output(output, ctx) end)
    |> Enum.reject(&(&1 == :ignored))
    |> case do
      [] -> :ignored
      rendered -> Enum.intersperse(rendered, "\n\n")
    end
  end

  defp render_output(_output, _ctx), do: :ignored

  defp encode_js_data(data) when is_binary(data), do: {:ok, data}
  defp encode_js_data(data), do: data |> ensure_order() |> Jason.encode()

  defp render_metadata(metadata) do
    metadata_json = metadata |> ensure_order() |> Jason.encode!()
    ["<!-- livebook:", metadata_json, " -->"]
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

  defp render_notebook_footer(_notebook, _notebook_source, _include_stamp? = false), do: {[], []}

  defp render_notebook_footer(notebook, notebook_source, true) do
    metadata = notebook_stamp_metadata(notebook)

    case Livebook.Hubs.fetch_hub(notebook.hub_id) do
      {:ok, hub} ->
        case Livebook.Hubs.notebook_stamp(hub, notebook_source, metadata) do
          {:ok, stamp} ->
            offset = IO.iodata_length(notebook_source)
            json = %{"offset" => offset, "stamp" => stamp} |> ensure_order() |> Jason.encode!()
            footer = ["\n", "<!-- livebook:", json, " -->", "\n"]
            {footer, []}

          :skip ->
            {[], []}

          {:error, message} ->
            {[], ["failed to stamp the notebook, #{message}"]}
        end

      :error ->
        {[], []}
    end
  end

  defp notebook_stamp_metadata(notebook) do
    keys = [:hub_secret_names]

    metadata = put_unless_default(%{}, Map.take(notebook, keys), Map.take(Notebook.new(), keys))

    # If there are any :file file entries, we want to generate a stamp
    # to make sure the entries are not tampered with. We also want to
    # store the information about file entries already in quarantine
    metadata =
      if Enum.any?(notebook.file_entries, &(&1.type == :file)) do
        Map.put(
          metadata,
          :quarantine_file_entry_names,
          MapSet.to_list(notebook.quarantine_file_entry_names)
        )
      else
        metadata
      end

    if notebook.app_settings.slug != nil and notebook.app_settings.access_type == :protected do
      Map.put(metadata, :app_settings_password, notebook.app_settings.password)
    else
      metadata
    end
  end

  defp ensure_order(%{} = map) when not is_struct(map) do
    map
    |> Enum.sort()
    |> Enum.map(fn {key, value} -> {key, ensure_order(value)} end)
    |> Jason.OrderedObject.new()
  end

  defp ensure_order(list) when is_list(list) do
    Enum.map(list, &ensure_order/1)
  end

  defp ensure_order(term), do: term
end
