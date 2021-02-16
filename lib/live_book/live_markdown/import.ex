defmodule LiveBook.LiveMarkdown.Import do
  alias LiveBook.Notebook
  alias LiveBook.LiveMarkdown.MarkdownHelpers

  @doc """
  Converts the given Markdown document into a notebook data structure.
  """
  @spec notebook_from_markdown(String.t()) :: Notebook.t()
  def notebook_from_markdown(markdown) do
    {_, ast, _} = EarmarkParser.as_ast(markdown)

    ast
    |> rewrite_ast()
    |> group_elements()
    |> build_notebook()
  end

  # Does initial pre-processing of the AST, so that it conforms to the expected form.
  defp rewrite_ast(ast) do
    ast
    |> rewrite_multiple_primary_headings()
  end

  # There should be only one h1 tag indicating notebook name,
  # if there are many we downgrade all headings.
  # This doesn't apply to documents exported from LiveBook,
  # but may be the case for an arbitrary markdown file,
  # so we do our best to preserve the intent.
  defp rewrite_multiple_primary_headings(ast) do
    primary_headings = Enum.count(ast, fn {tag, _, _, _} -> tag == "h1" end)

    if primary_headings > 1 do
      Enum.map(ast, &downgrade_heading/1)
    else
      ast
    end
  end

  defp downgrade_heading({"h1", attrs, content, meta}), do: {"h2", attrs, content, meta}
  defp downgrade_heading({"h2", attrs, content, meta}), do: {"h3", attrs, content, meta}
  defp downgrade_heading({"h3", attrs, content, meta}), do: {"h4", attrs, content, meta}
  defp downgrade_heading({"h4", attrs, content, meta}), do: {"h5", attrs, content, meta}
  defp downgrade_heading({"h5", attrs, content, meta}), do: {"h6", attrs, content, meta}
  defp downgrade_heading({"h6", attrs, content, meta}), do: {"strong", attrs, content, meta}
  defp downgrade_heading(ast_node), do: ast_node

  # Builds a list of classified elements from the AST.
  defp group_elements(ast, elems \\ [])

  defp group_elements([], elems), do: elems

  defp group_elements([{"h1", _, content, %{}} | ast], elems) do
    group_elements(ast, [{:notebook_name, content} | elems])
  end

  defp group_elements([{"h2", _, content, %{}} | ast], elems) do
    group_elements(ast, [{:section_name, content} | elems])
  end

  defp group_elements([{:comment, _, ["live_book:" <> metadata_json], %{comment: true}} | ast], elems) do
    group_elements(ast, [{:metadata, metadata_json} | elems])
  end

  defp group_elements(
         [{"pre", _, [{"code", [{"class", "elixir"}], [source], %{}}], %{}} | ast],
         elems
       ) do
    group_elements(ast, [{:cell, :elixir, source} | elems])
  end

  defp group_elements([ast_node | ast], [{:cell, :markdown, md_ast} | rest]) do
    group_elements(ast, [{:cell, :markdown, [ast_node | md_ast]} | rest])
  end

  defp group_elements([ast_node | ast], elems) do
    group_elements(ast, [{:cell, :markdown, [ast_node]} | elems])
  end

  # Builds a notebook from the list of elements obtained in the previous step.
  # Note that the list of elements is reversed:
  # first we group elements by traversing Earmark AST top-down
  # and then aggregate elements into data strictures going bottom-up.
  defp build_notebook(elems, cells \\ [], sections \\ [])

  defp build_notebook([{:cell, :elixir, source} | elems], cells, sections) do
    {metadata, elems} = grab_metadata(elems)
    cell = %{Notebook.Cell.new(:elixir) | source: source, metadata: metadata}
    build_notebook(elems, [cell | cells], sections)
  end

  defp build_notebook([{:cell, :markdown, md_ast} | elems], cells, sections) do
    {metadata, elems} = grab_metadata(elems)
    source = md_ast |> Enum.reverse() |> MarkdownHelpers.Renderer.markdown_from_ast()
    cell = %{Notebook.Cell.new(:markdown) | source: source, metadata: metadata}
    build_notebook(elems, [cell | cells], sections)
  end

  defp build_notebook([{:section_name, content} | elems], cells, sections) do
    name = MarkdownHelpers.text_from_ast(content)
    {metadata, elems} = grab_metadata(elems)
    section = %{Notebook.Section.new() | name: name, cells: cells, metadata: metadata}
    build_notebook(elems, [], [section | sections])
  end

  # If there are section-less cells, put them in a default one.
  defp build_notebook([{:notebook_name, _content} | _] = elems, cells, sections)
       when cells != [] do
    section = %{Notebook.Section.new() | cells: cells}
    build_notebook(elems, [], [section | sections])
  end

  # If there are section-less cells, put them in a default one.
  defp build_notebook([] = elems, cells, sections) when cells != [] do
    section = %{Notebook.Section.new() | cells: cells}
    build_notebook(elems, [], [section | sections])
  end

  defp build_notebook([{:notebook_name, content} | elems], [], sections) do
    name = MarkdownHelpers.text_from_ast(content)
    {metadata, []} = grab_metadata(elems)
    %{Notebook.new() | name: name, sections: sections, metadata: metadata}
  end

  # If there's no explicit notebook heading, use the defaults.
  defp build_notebook([], [], sections) do
    %{Notebook.new() | sections: sections}
  end

  # Takes optional leading metadata JSON object and returns {metadata, rest}.
  defp grab_metadata([{:metadata, metadata_json} | elems]) do
    metadata = Jason.decode!(metadata_json)
    {metadata, elems}
  end

  defp grab_metadata(elems), do: {%{}, elems}
end
