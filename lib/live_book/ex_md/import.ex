defmodule LiveBook.ExMd.Import do
  alias LiveBook.ExMd.MarkdownRenderer
  alias LiveBook.Notebook

  def markdown_to_notebook(markdown) do
    {:ok, ast, _} = EarmarkParser.as_ast(markdown)

    ast
    |> walk_ast([])
    |> to_notebook()
  end

  defp walk_ast([{"h1", _, [content], _} | ast], elems) do
    if Enum.all?(elems, &(elem(&1, 0) == :metaentry)) do
      walk_ast(ast, [{:notebook, content} | elems])
    else
      # TODO: error tuple or something?
      raise "Unexpected h1"
    end
  end

  defp walk_ast([], elems), do: elems

  defp walk_ast([{"h2", _, [content], _} | ast], elems) do
    walk_ast(ast, [{:section, content} | elems])
  end

  defp walk_ast([{:comment, _, ["live_book:" <> metaentry], %{comment: true}} | ast], elems) do
    walk_ast(ast, [{:metaentry, metaentry} | elems])
  end

  defp walk_ast([{"pre", [], [{"code", [{"class", "elixir"}], [content], _}], _} | ast], elems) do
    walk_ast(ast, [{:cell, :elixir, content} | elems])
  end

  defp walk_ast([ast_node | ast], [{:cell, :markdown, md_ast} | rest]) do
    walk_ast(ast, [{:cell, :markdown, [ast_node | md_ast]} | rest])
  end

  defp walk_ast([ast_node | ast], elems) do
    walk_ast(ast, [{:cell, :markdown, [ast_node]} | elems])
  end

  # ---

  defp to_notebook(elems, cells \\ [], sections \\ [])

  defp to_notebook([{:cell, :elixir, source} | elems], cells, sections) do
    {metadata, elems} = grab_metadata(elems)
    cell = %{Notebook.Cell.new(:elixir) | source: source, metadata: metadata}
    to_notebook(elems, [cell | cells], sections)
  end

  defp to_notebook([{:cell, :markdown, md_ast} | elems], cells, sections) do
    {metadata, elems} = grab_metadata(elems)
    source = md_ast |> Enum.reverse() |> MarkdownRenderer.markdown_from_ast()
    cell = %{Notebook.Cell.new(:markdown) | source: source, metadata: metadata}
    to_notebook(elems, [cell | cells], sections)
  end

  defp to_notebook([{:section, name} | elems], cells, sections) do
    {metadata, elems} = grab_metadata(elems)
    section = %{Notebook.Section.new() | name: name, cells: cells, metadata: metadata}
    to_notebook(elems, [], [section | sections])
  end

  # TODO: handle default title if no notebook
  defp to_notebook([{:notebook, name} | elems], [], sections) do
    {metadata, _elems} = grab_metadata(elems)
    %{Notebook.new() | name: name, sections: sections, metadata: metadata}
  end

  # ---

  defp grab_metadata(elems, metadata \\ %{})

  defp grab_metadata([{:metaentry, metaentry} | elems], metadata) do
    {key, value} = parse_metaentry(metaentry)
    grab_metadata(elems, Map.put(metadata, key, value))
  end

  defp grab_metadata(elems, metadata), do: {metadata, elems}

  defp parse_metaentry(metaentry) do
    [key_string, value_json] = String.split(metaentry, ":", parts: 2)
    key = String.to_atom(key_string)
    value = Jason.decode!(value_json)
    {key, value}
  end
end
