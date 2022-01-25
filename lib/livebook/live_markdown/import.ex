defmodule Livebook.LiveMarkdown.Import do
  alias Livebook.Notebook
  alias Livebook.LiveMarkdown.MarkdownHelpers

  @doc """
  Converts the given Markdown document into a notebook data structure.

  Returns the notebook structure and a list of informative messages/warnings
  related to the imported input.
  """
  @spec notebook_from_markdown(String.t()) :: {Notebook.t(), list(String.t())}
  def notebook_from_markdown(markdown) do
    {_, ast, earmark_messages} = MarkdownHelpers.markdown_to_block_ast(markdown)
    earmark_messages = Enum.map(earmark_messages, &earmark_message_to_string/1)

    {ast, rewrite_messages} = rewrite_ast(ast)
    elements = group_elements(ast)
    {notebook, build_messages} = build_notebook(elements)
    notebook = postprocess_notebook(notebook)

    {notebook, earmark_messages ++ rewrite_messages ++ build_messages}
  end

  defp earmark_message_to_string({_severity, line_number, message}) do
    "Line #{line_number}: #{message}"
  end

  # Does initial pre-processing of the AST, so that it conforms to the expected form.
  # Returns {altered_ast, messages}.
  defp rewrite_ast(ast) do
    {ast, messages1} = rewrite_multiple_primary_headings(ast)
    {ast, messages2} = move_primary_heading_top(ast)
    ast = normalize_comments(ast)

    {ast, messages1 ++ messages2}
  end

  # There should be only one h1 tag indicating notebook name,
  # if there are many we downgrade all headings.
  # This doesn't apply to documents exported from Livebook,
  # but may be the case for an arbitrary markdown file,
  # so we do our best to preserve the intent.
  defp rewrite_multiple_primary_headings(ast) do
    primary_headings = Enum.count(ast, &(tag(&1) == "h1"))

    if primary_headings > 1 do
      ast = Enum.map(ast, &downgrade_heading/1)

      message =
        "Downgrading all headings, because #{primary_headings} instances of heading 1 were found"

      {ast, [message]}
    else
      {ast, []}
    end
  end

  defp downgrade_heading({"h1", attrs, content, meta}), do: {"h2", attrs, content, meta}
  defp downgrade_heading({"h2", attrs, content, meta}), do: {"h3", attrs, content, meta}
  defp downgrade_heading({"h3", attrs, content, meta}), do: {"h4", attrs, content, meta}
  defp downgrade_heading({"h4", attrs, content, meta}), do: {"h5", attrs, content, meta}
  defp downgrade_heading({"h5", attrs, content, meta}), do: {"h6", attrs, content, meta}
  defp downgrade_heading({"h6", attrs, content, meta}), do: {"strong", attrs, content, meta}
  defp downgrade_heading(ast_node), do: ast_node

  # This moves h1 together with any preceding comments to the top.
  defp move_primary_heading_top(ast) do
    case Enum.split_while(ast, &(tag(&1) != "h1")) do
      {_ast, []} ->
        {ast, []}

      {leading, [heading | rest]} ->
        {leading, comments} = split_while_right(leading, &(tag(&1) == :comment))

        if leading == [] do
          {ast, []}
        else
          ast = comments ++ [heading] ++ leading ++ rest
          message = "Moving heading 1 to the top of the notebook"
          {ast, [message]}
        end
    end
  end

  defp tag(ast_node)
  defp tag({tag, _, _, _}), do: tag
  defp tag(_), do: nil

  defp split_while_right(list, fun) do
    {right_rev, left_rev} = list |> Enum.reverse() |> Enum.split_while(fun)
    {Enum.reverse(left_rev), Enum.reverse(right_rev)}
  end

  # Normalizes comments to allow nice pattern matching on Livebook-specific
  # annotations with no regard to surrounding whitespace.
  defp normalize_comments(ast) do
    Enum.map(ast, fn
      {:comment, attrs, lines, %{comment: true}} ->
        {:comment, attrs, MarkdownHelpers.normalize_comment_lines(lines), %{comment: true}}

      ast_node ->
        ast_node
    end)
  end

  # Builds a list of classified elements from the AST.
  defp group_elements(ast, elems \\ [])

  defp group_elements([], elems), do: elems

  defp group_elements([{"h1", _, [content], %{}} | ast], elems) do
    group_elements(ast, [{:notebook_name, content} | elems])
  end

  defp group_elements([{"h2", _, [content], %{}} | ast], elems) do
    group_elements(ast, [{:section_name, content} | elems])
  end

  # The <!-- livebook:{"force_markdown":true} --> annotation forces the next node
  # to be interpreted as Markdown cell content.
  defp group_elements(
         [
           {:comment, _, [~s/livebook:{"force_markdown":true}/], %{comment: true}},
           ast_node | ast
         ],
         [{:cell, :markdown, md_ast} | rest]
       ) do
    group_elements(ast, [{:cell, :markdown, [ast_node | md_ast]} | rest])
  end

  defp group_elements(
         [
           {:comment, _, [~s/livebook:{"force_markdown":true}/], %{comment: true}},
           ast_node | ast
         ],
         elems
       ) do
    group_elements(ast, [{:cell, :markdown, [ast_node]} | elems])
  end

  defp group_elements(
         [{:comment, _, [~s/livebook:{"break_markdown":true}/], %{comment: true}} | ast],
         elems
       ) do
    group_elements(ast, [{:cell, :markdown, []} | elems])
  end

  defp group_elements(
         [{:comment, _, ["livebook:" <> json], %{comment: true}} | ast],
         elems
       ) do
    group_elements(ast, [livebook_json_to_element(json) | elems])
  end

  defp group_elements(
         [{"pre", _, [{"code", [{"class", "elixir"}], [source], %{}}], %{}} | ast],
         elems
       ) do
    {outputs, ast} = take_outputs(ast, [])
    group_elements(ast, [{:cell, :elixir, source, outputs} | elems])
  end

  defp group_elements([ast_node | ast], [{:cell, :markdown, md_ast} | rest]) do
    group_elements(ast, [{:cell, :markdown, [ast_node | md_ast]} | rest])
  end

  defp group_elements([ast_node | ast], elems) do
    group_elements(ast, [{:cell, :markdown, [ast_node]} | elems])
  end

  defp livebook_json_to_element(json) do
    data = Jason.decode!(json)

    case data do
      %{"livebook_object" => "cell_input"} ->
        {:cell, :input, data}

      _ ->
        {:metadata, data}
    end
  end

  # Import ```output snippets for backward compatibility
  defp take_outputs(
         [{"pre", _, [{"code", [{"class", "output"}], [output], %{}}], %{}} | ast],
         outputs
       ) do
    take_outputs(ast, [{:text, output} | outputs])
  end

  defp take_outputs(
         [
           {:comment, _, [~s/livebook:{"output":true}/], %{comment: true}},
           {"pre", _, [{"code", [], [output], %{}}], %{}}
           | ast
         ],
         outputs
       ) do
    take_outputs(ast, [{:text, output} | outputs])
  end

  # Ignore other exported outputs
  defp take_outputs(
         [
           {:comment, _, [~s/livebook:{"output":true}/], %{comment: true}},
           {"pre", _, [{"code", [{"class", _info_string}], [_output], %{}}], %{}}
           | ast
         ],
         outputs
       ) do
    take_outputs(ast, outputs)
  end

  defp take_outputs(ast, outputs), do: {outputs, ast}

  # Builds a notebook from the list of elements obtained in the previous step.
  # Note that the list of elements is reversed:
  # first we group elements by traversing Earmark AST top-down
  # and then aggregate elements into data strictures going bottom-up.
  defp build_notebook(elems) do
    build_notebook(elems, _cells = [], _sections = [], _messages = [], _output_counter = 0)
  end

  defp build_notebook(
         [{:cell, :elixir, source, outputs} | elems],
         cells,
         sections,
         messages,
         output_counter
       ) do
    {metadata, elems} = grab_metadata(elems)
    attrs = cell_metadata_to_attrs(:elixir, metadata)
    {outputs, output_counter} = Notebook.index_outputs(outputs, output_counter)
    cell = %{Notebook.Cell.new(:elixir) | source: source, outputs: outputs} |> Map.merge(attrs)
    build_notebook(elems, [cell | cells], sections, messages, output_counter)
  end

  defp build_notebook(
         [{:cell, :markdown, md_ast} | elems],
         cells,
         sections,
         messages,
         output_counter
       ) do
    {metadata, elems} = grab_metadata(elems)
    attrs = cell_metadata_to_attrs(:markdown, metadata)
    source = md_ast |> Enum.reverse() |> MarkdownHelpers.markdown_from_ast()
    cell = %{Notebook.Cell.new(:markdown) | source: source} |> Map.merge(attrs)
    build_notebook(elems, [cell | cells], sections, messages, output_counter)
  end

  defp build_notebook([{:cell, :input, data} | elems], cells, sections, messages, output_counter) do
    warning =
      "found an input cell, but those are no longer supported, please use Kino.Input instead"

    warning =
      if data["reactive"] == true do
        warning <>
          ". Also, to make the input reactive you can use an automatically reevaluating cell"
      else
        warning
      end

    build_notebook(elems, cells, sections, messages ++ [warning], output_counter)
  end

  defp build_notebook(
         [{:section_name, name} | elems],
         cells,
         sections,
         messages,
         output_counter
       ) do
    {metadata, elems} = grab_metadata(elems)
    attrs = section_metadata_to_attrs(metadata)
    section = %{Notebook.Section.new() | name: name, cells: cells} |> Map.merge(attrs)
    build_notebook(elems, [], [section | sections], messages, output_counter)
  end

  # If there are section-less cells, put them in a default one.
  defp build_notebook(
         [{:notebook_name, _name} | _] = elems,
         cells,
         sections,
         messages,
         output_counter
       )
       when cells != [] do
    section = %{Notebook.Section.new() | cells: cells}
    build_notebook(elems, [], [section | sections], messages, output_counter)
  end

  # If there are section-less cells, put them in a default one.
  defp build_notebook([] = elems, cells, sections, messages, output_counter) when cells != [] do
    section = %{Notebook.Section.new() | cells: cells}
    build_notebook(elems, [], [section | sections], messages, output_counter)
  end

  defp build_notebook([{:notebook_name, name} | elems], [], sections, messages, output_counter) do
    {metadata, elems} = grab_metadata(elems)
    # If there are any non-metadata comments we keep them
    {comments, elems} = grab_leading_comments(elems)

    messages =
      if elems == [] do
        messages
      else
        messages ++
          [
            "found an invalid sequence of comments at the beginning, make sure custom comments are at the very top"
          ]
      end

    attrs = notebook_metadata_to_attrs(metadata)

    notebook =
      %{
        Notebook.new()
        | name: name,
          sections: sections,
          leading_comments: comments,
          output_counter: output_counter
      }
      |> Map.merge(attrs)

    {notebook, messages}
  end

  # If there's no explicit notebook heading, use the defaults.
  defp build_notebook([], [], sections, messages, output_counter) do
    notebook = %{Notebook.new() | sections: sections, output_counter: output_counter}
    {notebook, messages}
  end

  # Takes optional leading metadata JSON object and returns {metadata, rest}.
  defp grab_metadata([{:metadata, metadata} | elems]) do
    {metadata, elems}
  end

  defp grab_metadata(elems), do: {%{}, elems}

  defp grab_leading_comments([]), do: {[], []}

  # Since these are not metadata comments they get wrapped in a markdown cell,
  # so we unpack it
  defp grab_leading_comments([{:cell, :markdown, md_ast}]) do
    comments = for {:comment, _attrs, lines, %{comment: true}} <- Enum.reverse(md_ast), do: lines
    {comments, []}
  end

  defp grab_leading_comments(elems), do: {[], elems}

  defp notebook_metadata_to_attrs(metadata) do
    Enum.reduce(metadata, %{}, fn
      {"persist_outputs", persist_outputs}, attrs ->
        Map.put(attrs, :persist_outputs, persist_outputs)

      {"autosave_interval_s", autosave_interval_s}, attrs ->
        Map.put(attrs, :autosave_interval_s, autosave_interval_s)

      _entry, attrs ->
        attrs
    end)
  end

  defp section_metadata_to_attrs(metadata) do
    Enum.reduce(metadata, %{}, fn
      {"branch_parent_index", parent_idx}, attrs ->
        # At this point we cannot extract other section id,
        # so we temporarily keep the index
        Map.put(attrs, :parent_id, {:idx, parent_idx})

      _entry, attrs ->
        attrs
    end)
  end

  defp cell_metadata_to_attrs(:elixir, metadata) do
    Enum.reduce(metadata, %{}, fn
      {"disable_formatting", disable_formatting}, attrs ->
        Map.put(attrs, :disable_formatting, disable_formatting)

      {"reevaluate_automatically", reevaluate_automatically}, attrs ->
        Map.put(attrs, :reevaluate_automatically, reevaluate_automatically)

      _entry, attrs ->
        attrs
    end)
  end

  defp cell_metadata_to_attrs(_type, _metadata) do
    %{}
  end

  defp postprocess_notebook(notebook) do
    sections =
      Enum.map(notebook.sections, fn section ->
        # Set parent_id based on the persisted branch_parent_index if present
        case section.parent_id do
          nil ->
            section

          {:idx, parent_idx} ->
            parent = Enum.at(notebook.sections, parent_idx)
            %{section | parent_id: parent.id}
        end
      end)

    %{notebook | sections: sections}
  end
end
