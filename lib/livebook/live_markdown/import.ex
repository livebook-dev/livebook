defmodule Livebook.LiveMarkdown.Import do
  alias Livebook.Hubs
  alias Livebook.Notebook
  alias Livebook.LiveMarkdown.MarkdownHelpers

  def notebook_from_livemd(markdown) do
    {_, ast, earmark_messages} = MarkdownHelpers.markdown_to_block_ast(markdown)
    earmark_messages = Enum.map(earmark_messages, &earmark_message_to_string/1)

    {ast, rewrite_messages} = rewrite_ast(ast)
    elements = group_elements(ast)
    {stamp_data, elements} = take_stamp_data(elements)
    {notebook, valid_hub?, build_messages} = build_notebook(elements)
    {notebook, postprocess_messages} = postprocess_notebook(notebook)

    {notebook, stamp_verified?, metadata_messages} =
      if stamp_data != nil and valid_hub? do
        postprocess_stamp(notebook, markdown, stamp_data)
      else
        {notebook, false, []}
      end

    messages =
      earmark_messages ++
        rewrite_messages ++ build_messages ++ postprocess_messages ++ metadata_messages

    {notebook, %{warnings: messages, stamp_verified?: stamp_verified?}}
  end

  defp earmark_message_to_string({_severity, line_number, message}) do
    "line #{line_number} - #{Livebook.Utils.downcase_first(message)}"
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
        "downgrading all headings, because #{primary_headings} instances of heading 1 were found"

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
          message = "moving heading 1 to the top of the notebook"
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
         [{"pre", _, [{"code", [{"class", language}], [source], %{}}], %{}} | ast],
         elems
       )
       when language in ["elixir", "erlang", "python", "pyproject.toml"] do
    {outputs, ast} = take_outputs(ast, [])

    language = String.to_atom(language)

    group_elements(ast, [{:cell, :code, language, source, outputs} | elems])
  end

  defp group_elements([ast_node | ast], [{:cell, :markdown, md_ast} | rest]) do
    group_elements(ast, [{:cell, :markdown, [ast_node | md_ast]} | rest])
  end

  defp group_elements([ast_node | ast], elems) do
    group_elements(ast, [{:cell, :markdown, [ast_node]} | elems])
  end

  defp livebook_json_to_element(json) do
    data = JSON.decode!(json)

    case data do
      %{"livebook_object" => "cell_input"} ->
        {:cell, :input, data}

      %{"livebook_object" => "smart_cell"} ->
        {:cell, :smart, data}

      %{"stamp" => _} ->
        {:stamp, data}

      _ ->
        {:metadata, data}
    end
  end

  # Import ```output snippets for backward compatibility
  defp take_outputs(
         [{"pre", _, [{"code", [{"class", "output"}], [output], %{}}], %{}} | ast],
         outputs
       ) do
    take_outputs(ast, [%{type: :terminal_text, text: output, chunk: false} | outputs])
  end

  defp take_outputs(
         [
           {:comment, _, [~s/livebook:{"output":true}/], %{comment: true}},
           {"pre", _, [{"code", [], [output], %{}}], %{}}
           | ast
         ],
         outputs
       ) do
    take_outputs(ast, [%{type: :terminal_text, text: output, chunk: false} | outputs])
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

  # Builds a notebook from the list of elements obtained in the
  # previous step. The elements are in reversed order, because we
  # want to aggregate them into data structures going bottom-up.
  defp build_notebook(elems) do
    build_notebook(elems, _cells = [], _sections = [], _messages = [], _output_counter = 0)
  end

  defp build_notebook(
         [{:cell, :code, :elixir, source, outputs}, {:cell, :smart, data} | elems],
         cells,
         sections,
         messages,
         output_counter
       ) do
    {outputs, output_counter} = Notebook.index_outputs(outputs, output_counter)
    %{"kind" => kind, "attrs" => attrs} = data

    attrs =
      case attrs do
        # Import map attributes for backward compatibility
        %{} ->
          attrs

        encoded when is_binary(encoded) ->
          encoded |> Base.decode64!(padding: false) |> JSON.decode!()
      end

    chunks = if(chunks = data["chunks"], do: Enum.map(chunks, &List.to_tuple/1))

    cell = %{
      Notebook.Cell.new(:smart)
      | source: source,
        chunks: chunks,
        outputs: outputs,
        kind: kind,
        attrs: attrs
    }

    build_notebook(elems, [cell | cells], sections, messages, output_counter)
  end

  defp build_notebook(
         [{:cell, :code, language, source, outputs} | elems],
         cells,
         sections,
         messages,
         output_counter
       ) do
    {metadata, elems} = grab_metadata(elems)
    attrs = cell_metadata_to_attrs(:code, metadata)
    {outputs, output_counter} = Notebook.index_outputs(outputs, output_counter)

    cell =
      %{Notebook.Cell.new(:code) | source: source, language: language, outputs: outputs}
      |> Map.merge(attrs)

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

  defp build_notebook([{:section_name, name} | elems], cells, sections, messages, output_counter) do
    {metadata, elems} = grab_metadata(elems)
    attrs = section_metadata_to_attrs(metadata)
    section = %{Notebook.Section.new() | name: name, cells: cells} |> Map.merge(attrs)
    build_notebook(elems, [], [section | sections], messages, output_counter)
  end

  @unknown_hub_message "this notebook belongs to an Organization you don't have access to. " <>
                         "Head to Livebook's home and add its Organization before reopening this notebook"

  defp build_notebook(elems, cells, sections, messages, output_counter) do
    # At this point we expect the heading, otherwise we use the default
    {name, elems} =
      case elems do
        [{:notebook_name, name} | elems] -> {name, elems}
        [] -> {nil, []}
      end

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

    {attrs, messages} = notebook_metadata_to_attrs(metadata, messages)
    hub_id = attrs[:hub_id]

    {attrs, valid_hub?, messages} =
      if is_nil(hub_id) or Hubs.hub_exists?(hub_id) do
        {attrs, true, messages}
      else
        {Map.drop(attrs, [:hub_id, :deployment_group_id]), false,
         messages ++ [@unknown_hub_message]}
      end

    # Check if the remaining cells form a valid setup section, otherwise
    # we put them into a default section instead
    {setup_cells, extra_sections} =
      case cells do
        [%Notebook.Cell.Code{language: :elixir}] when name != nil ->
          {cells, []}

        [%Notebook.Cell.Code{language: :elixir}, %Notebook.Cell.Code{language: :"pyproject.toml"}]
        when name != nil ->
          {cells, []}

        [] ->
          {nil, []}

        extra_cells ->
          {nil, [%{Notebook.Section.new() | cells: extra_cells}]}
      end

    notebook =
      %{
        Notebook.new()
        | sections: extra_sections ++ sections,
          leading_comments: comments,
          output_counter: output_counter
      }
      |> maybe_put_name(name)
      |> maybe_put_setup_cells(setup_cells)
      |> Map.merge(attrs)

    {notebook, valid_hub?, messages}
  end

  defp maybe_put_name(notebook, nil), do: notebook
  defp maybe_put_name(notebook, name), do: %{notebook | name: name}

  defp maybe_put_setup_cells(notebook, nil), do: notebook
  defp maybe_put_setup_cells(notebook, cells), do: Notebook.put_setup_cells(notebook, cells)

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

  defp notebook_metadata_to_attrs(metadata, messages) do
    Enum.reduce(metadata, {%{}, messages}, fn
      {"persist_outputs", persist_outputs}, {attrs, messages} ->
        {Map.put(attrs, :persist_outputs, persist_outputs), messages}

      {"autosave_interval_s", autosave_interval_s}, {attrs, messages} ->
        {Map.put(attrs, :autosave_interval_s, autosave_interval_s), messages}

      {"default_language", default_language}, {attrs, messages}
      when default_language in ["elixir", "erlang"] ->
        default_language = String.to_atom(default_language)
        {Map.put(attrs, :default_language, default_language), messages}

      {"hub_id", hub_id}, {attrs, messages} ->
        {Map.put(attrs, :hub_id, hub_id), messages}

      {"deployment_group_id", deployment_group_id}, {attrs, messages} ->
        {Map.put(attrs, :deployment_group_id, deployment_group_id), messages}

      {"app_settings", app_settings_metadata}, {attrs, messages} ->
        app_settings =
          Map.merge(
            Notebook.AppSettings.new(),
            app_settings_metadata_to_attrs(app_settings_metadata)
          )

        {Map.put(attrs, :app_settings, app_settings), messages}

      {"file_entries", file_entry_metadata}, {attrs, messages}
      when is_list(file_entry_metadata) ->
        {file_entries, file_entry_messages} =
          for file_entry_metadata <- file_entry_metadata, reduce: {[], []} do
            {file_entries, warnings} ->
              case file_entry_metadata_to_attrs(file_entry_metadata) do
                {:ok, file_entry} -> {[file_entry | file_entries], warnings}
                {:error, message} -> {file_entries, [message | warnings]}
              end
          end

        # By default we put all :file entries in quarantine, if there
        # is a valid stamp, we override this later
        quarantine_file_entry_names =
          for entry <- file_entries, entry.type == :file, into: MapSet.new(), do: entry.name

        attrs =
          attrs
          |> Map.put(:file_entries, file_entries)
          |> Map.put(:quarantine_file_entry_names, quarantine_file_entry_names)

        {attrs, messages ++ file_entry_messages}

      _entry, {attrs, messages} ->
        {attrs, messages}
    end)
  end

  defp app_settings_metadata_to_attrs(metadata) do
    Enum.reduce(metadata, %{}, fn
      {"slug", slug}, attrs ->
        Map.put(attrs, :slug, slug)

      {"multi_session", multi_session}, attrs ->
        Map.put(attrs, :multi_session, multi_session)

      {"zero_downtime", zero_downtime}, attrs ->
        Map.put(attrs, :zero_downtime, zero_downtime)

      {"show_existing_sessions", show_existing_sessions}, attrs ->
        Map.put(attrs, :show_existing_sessions, show_existing_sessions)

      {"auto_shutdown_ms", auto_shutdown_ms}, attrs ->
        Map.put(attrs, :auto_shutdown_ms, auto_shutdown_ms)

      {"access_type", access_type}, attrs when access_type in ["public", "protected"] ->
        Map.put(attrs, :access_type, String.to_atom(access_type))

      {"show_source", show_source}, attrs ->
        Map.put(attrs, :show_source, show_source)

      {"output_type", output_type}, attrs when output_type in ["all", "rich"] ->
        Map.put(attrs, :output_type, String.to_atom(output_type))

      _entry, attrs ->
        attrs
    end)
  end

  defp file_entry_metadata_to_attrs(%{"type" => "attachment", "name" => name}) do
    {:ok, %{type: :attachment, name: name}}
  end

  defp file_entry_metadata_to_attrs(%{
         "type" => "file",
         "name" => name,
         "file" => %{
           "file_system_id" => file_system_id,
           "file_system_type" => file_system_type,
           "path" => path
         }
       }) do
    file = %Livebook.FileSystem.File{
      file_system_id: file_system_id,
      file_system_module: Livebook.FileSystems.type_to_module(file_system_type),
      path: path,
      origin_pid: self()
    }

    {:ok, %{type: :file, name: name, file: file}}
  end

  defp file_entry_metadata_to_attrs(%{"type" => "url", "name" => name, "url" => url}) do
    {:ok, %{type: :url, name: name, url: url}}
  end

  defp file_entry_metadata_to_attrs(_other) do
    {:error, "discarding file entry in invalid format"}
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

  defp cell_metadata_to_attrs(:code, metadata) do
    Enum.reduce(metadata, %{}, fn
      {"reevaluate_automatically", reevaluate_automatically}, attrs ->
        Map.put(attrs, :reevaluate_automatically, reevaluate_automatically)

      {"continue_on_error", continue_on_error}, attrs ->
        Map.put(attrs, :continue_on_error, continue_on_error)

      _entry, attrs ->
        attrs
    end)
  end

  defp cell_metadata_to_attrs(_type, _metadata) do
    %{}
  end

  defp postprocess_notebook(notebook) do
    {sections, {_branching_ids, warnings}} =
      notebook.sections
      |> Enum.with_index()
      |> Enum.map_reduce({MapSet.new(), []}, fn {section, section_idx},
                                                {branching_ids, warnings} ->
        # Set parent_id based on the persisted branch_parent_index if present
        case section.parent_id do
          nil ->
            {section, {branching_ids, warnings}}

          {:idx, parent_idx} ->
            parent = Enum.at(notebook.sections, parent_idx)

            cond do
              section_idx <= parent_idx ->
                {%{section | parent_id: nil},
                 {
                   branching_ids,
                   [
                     ~s{ignoring the parent section of "#{section.name}", because it comes later in the notebook}
                     | warnings
                   ]
                 }}

              !is_nil(parent) && MapSet.member?(branching_ids, parent.id) ->
                {%{section | parent_id: nil},
                 {
                   branching_ids,
                   [
                     ~s{ignoring the parent section of "#{section.name}", because it is itself a branching section}
                     | warnings
                   ]
                 }}

              true ->
                {%{section | parent_id: parent.id},
                 {MapSet.put(branching_ids, section.id), warnings}}
            end
        end
      end)

    notebook = %{notebook | sections: sections}

    legacy_images? =
      notebook
      |> Notebook.cells_with_section()
      |> Enum.any?(fn {cell, _section} ->
        # A heuristic to detect legacy image source
        is_struct(cell, Notebook.Cell.Markdown) and String.contains?(cell.source, "](images/")
      end)

    image_warnings =
      if legacy_images? do
        [
          "found Markdown images pointing to the images/ directory." <>
            " Using this directory has been deprecated, please use notebook files instead"
        ]
      else
        []
      end

    {notebook, Enum.reverse(warnings) ++ image_warnings}
  end

  defp take_stamp_data([{:stamp, data} | elements]), do: {data, elements}
  defp take_stamp_data(elements), do: {nil, elements}

  @personal_stamp_message "this notebook can only access environment variables defined in this machine (the notebook was either authored in another machine or changed outside of Livebook)"
  @org_stamp_message "invalid notebook stamp, disabling access to secrets and remote files (this may happen if you made changes to the notebook source outside of Livebook)"
  @too_recent_stamp_message "invalid notebook stamp, disabling access to secrets and remote files (the stamp has been generated using a more recent Livebook version, you need to upgrade)"

  defp postprocess_stamp(notebook, notebook_source, stamp_data) do
    hub = Hubs.fetch_hub!(notebook.hub_id)

    {stamp_verified?, notebook, messages} =
      with %{"offset" => offset, "stamp" => stamp} <- stamp_data,
           {:ok, notebook_source, rest_source} <- safe_binary_split(notebook_source, offset),
           {:ok, ^stamp_data} <- only_stamp_data(rest_source),
           {:ok, metadata} <- Livebook.Hubs.verify_notebook_stamp(hub, notebook_source, stamp) do
        notebook = apply_stamp_metadata(notebook, metadata)
        {true, notebook, []}
      else
        error ->
          message =
            cond do
              error == {:error, :too_recent_version} -> @too_recent_stamp_message
              notebook.hub_id == "personal-hub" -> @personal_stamp_message
              true -> @org_stamp_message
            end

          {false, notebook, [message]}
      end

    # If the hub is online, then by definition it is a valid hub,
    # so we enable team features. If the hub is offline, then
    # we can only enable team features if the stamp is valid
    # (which means the server signed with a private key and we
    # validate it against the public key).
    teams_enabled = is_struct(hub, Livebook.Hubs.Team) and (hub.offline == nil or stamp_verified?)

    {%{notebook | teams_enabled: teams_enabled}, stamp_verified?, messages}
  end

  defp safe_binary_split(binary, offset)
       when byte_size(binary) < offset,
       do: :error

  defp safe_binary_split(binary, offset) do
    size = byte_size(binary)
    {:ok, binary_slice(binary, 0, offset), binary_slice(binary, offset, size - offset)}
  end

  defp only_stamp_data(source) do
    {_, ast, _} = source |> String.trim() |> MarkdownHelpers.markdown_to_block_ast()
    {ast, _} = rewrite_ast(ast)

    case group_elements(ast) do
      [{:stamp, data}] -> {:ok, data}
      _ -> :error
    end
  end

  defp apply_stamp_metadata(notebook, metadata) do
    Enum.reduce(metadata, notebook, fn
      {:hub_secret_names, hub_secret_names}, notebook ->
        %{notebook | hub_secret_names: hub_secret_names}

      {:quarantine_file_entry_names, quarantine_file_entry_names}, notebook ->
        %{notebook | quarantine_file_entry_names: MapSet.new(quarantine_file_entry_names)}

      {:app_settings_password, password}, notebook ->
        put_in(notebook.app_settings.password, password)

      _entry, notebook ->
        notebook
    end)
  end
end
