defmodule Livebook.Session.DataSync do
  alias Livebook.Notebook.Cell
  alias Livebook.Notebook.Section

  defguardp is_cell(cell)
            when is_struct(cell, Cell.Code) or is_struct(cell, Cell.Markdown) or
                   is_struct(cell, Cell.Smart)

  @doc """
  Diffs data with an updated notebook and returns a list of operations
  that should be applied to data to arrive at the updated version.
  """
  @spec sync(
          Livebook.Session.Data.t(),
          Livebook.Notebook.t(),
          Livebook.Session.Data.client_id()
        ) :: list(Livebook.Session.Data.operation())
  def sync(data, updated_notebook, client_id) do
    sync_notebook_attrs(data.notebook, updated_notebook, client_id) ++
      sync_setup_section(data, updated_notebook, client_id) ++
      sync_sections_and_cells(data, updated_notebook, client_id)
  end

  defp sync_notebook_attrs(before_notebook, after_notebook, client_id) do
    syncable_fields = [:name, :leading_comments]

    updated_attrs = attrs_diff(before_notebook, after_notebook, syncable_fields)

    if updated_attrs == %{} do
      []
    else
      [{:set_notebook_attributes, client_id, updated_attrs}]
    end
  end

  defp attrs_diff(before_struct, after_struct, keys) do
    for key <- keys,
        Map.fetch!(before_struct, key) != Map.fetch!(after_struct, key),
        into: %{},
        do: {key, Map.fetch!(after_struct, key)}
  end

  defp sync_setup_section(data, after_notebook, client_id) do
    before_languages = Livebook.Notebook.enabled_languages(data.notebook)
    after_languages = Livebook.Notebook.enabled_languages(after_notebook)

    added_languages = after_languages -- before_languages
    removed_languages = before_languages -- after_languages

    language_ops =
      Enum.map(added_languages, &{:enable_language, client_id, &1}) ++
        Enum.map(removed_languages, &{:disable_language, client_id, &1})

    # We apply the operations, so that the cells match and we can diff
    # them one by one. Note that setup cell ids are deterministic.
    data =
      Enum.reduce(language_ops, data, fn op, data ->
        {:ok, data, _actions} = Livebook.Session.Data.apply_operation(data, op)
        data
      end)

    source_ops =
      sync_setup_cells(
        data.notebook.setup_section.cells,
        after_notebook.setup_section.cells,
        {data, client_id}
      )

    language_ops ++ source_ops
  end

  defp sync_setup_cells(
         [%{id: id} = before_cell | before_cells],
         [%{id: id} = after_cell | after_cells],
         {data, client_id}
       ) do
    if before_cell.source != after_cell.source do
      op = cell_delta_operation(before_cell, after_cell, data, client_id)
      [op | sync_setup_cells(before_cells, after_cells, {data, client_id})]
    else
      sync_setup_cells(before_cells, after_cells, {data, client_id})
    end
  end

  defp sync_setup_cells([], [], _acc), do: []

  defp sync_sections_and_cells(data, after_notebook, client_id) do
    data.notebook
    # Compute a diff over a flat list of notebook blocks.
    |> keyed_diff(after_notebook)
    # Detect delete/insert operation pairs that are actually a move.
    |> annotate_moves()
    # Detect subsequent delete + insert as an update if applicable.
    |> annotate_updates()
    # Build a list of data operations that applies the diff to data.
    |> diff_to_data_ops(data, client_id)
  end

  defp keyed_diff(before_notebook, after_notebook) do
    before_items = flat_items(before_notebook)
    after_items = flat_items(after_notebook)

    before_keys = Enum.map(before_items, &key/1)
    after_keys = Enum.map(after_items, &key/1)

    diff = List.myers_difference(before_keys, after_keys)
    diff = for {type, ops} <- diff, op <- ops, do: {type, op}

    annotate_keyed_diff(diff, before_items, after_items)
  end

  defp flat_items(notebook) do
    Enum.flat_map(notebook.sections, fn section ->
      [section | section.cells]
    end)
  end

  defp key(%Section{} = section), do: {:section, section.name, section.parent_id}
  defp key(%Cell.Markdown{} = cell), do: {:markdown_cell, cell.source}

  defp key(%Cell.Code{} = cell),
    do: {:code_cell, cell.source, Map.take(cell, cell_syncable_fields(cell))}

  # Note that smart cell attributes in memory may be different than
  # onces after notebook export end import (e.g. atoms are stringified).
  # We could encode them here, but source should be enough as a key,
  # since attrs generally determine the source.
  defp key(%Cell.Smart{} = cell), do: {:smart_cell, cell.source}

  defp cell_syncable_fields(%Cell.Code{}),
    do: [:language, :reevaluate_automatically, :continue_on_error, :output_size]

  defp cell_syncable_fields(%Cell.Smart{}), do: [:output_size]

  defp cell_syncable_fields(_other), do: []

  defp annotate_keyed_diff(
         [{:eq, key} | diff],
         [before_item | before_items],
         [after_item | after_items]
       ) do
    [{:eq, key, before_item, after_item} | annotate_keyed_diff(diff, before_items, after_items)]
  end

  defp annotate_keyed_diff([{:ins, key} | diff], before_items, [after_item | after_items]) do
    [{:ins, key, after_item} | annotate_keyed_diff(diff, before_items, after_items)]
  end

  defp annotate_keyed_diff([{:del, key} | diff], [before_item | before_items], after_items) do
    [{:del, key, before_item} | annotate_keyed_diff(diff, before_items, after_items)]
  end

  defp annotate_keyed_diff([], [], []), do: []

  defp annotate_moves(diff) do
    ops_map =
      Enum.reduce(diff, %{}, fn
        {:ins, key, item}, acc ->
          Map.update(acc, {:ins, key}, [item], &(&1 ++ [item]))

        {:del, key, item}, acc ->
          Map.update(acc, {:del, key}, [item], &(&1 ++ [item]))

        _other, acc ->
          acc
      end)

    do_annotate_moves(diff, ops_map)
  end

  defp do_annotate_moves([{:ins, key, item} | diff], ops_map) when is_cell(item) do
    {op, ops_map} =
      case Map.fetch(ops_map, {:del, key}) do
        {:ok, [delete_item]} ->
          {{:move_ins, key, delete_item}, Map.delete(ops_map, {:del, key})}

        {:ok, [delete_item | delete_items]} ->
          {{:move_ins, key, delete_item}, Map.replace!(ops_map, {:del, key}, delete_items)}

        :error ->
          {{:ins, key, item}, ops_map}
      end

    [op | do_annotate_moves(diff, ops_map)]
  end

  defp do_annotate_moves([{:del, key, item} | diff], ops_map) when is_cell(item) do
    {op, ops_map} =
      case Map.fetch(ops_map, {:ins, key}) do
        {:ok, [_insert_item]} ->
          {{:move_del, key, item}, Map.delete(ops_map, {:ins, key})}

        {:ok, [_insert_item | insert_items]} ->
          {{:move_del, key, item}, Map.replace!(ops_map, {:ins, key}, insert_items)}

        :error ->
          {{:del, key, item}, ops_map}
      end

    [op | do_annotate_moves(diff, ops_map)]
  end

  defp do_annotate_moves([op | diff], ops_map) do
    [op | do_annotate_moves(diff, ops_map)]
  end

  defp do_annotate_moves([], _ops_map), do: []

  defp annotate_updates(ops) do
    # We could simply match on consecutive :del and :ins, however the
    # diff may include a sequence of :del ops and then a sequence of
    # :ins ops. They may not match in a zip way either, for example,
    # the first :del may not match the insert, but the second :del
    # does, so we should keep the first :del and make thse second one
    # into an :upd.
    #
    # To do this, we go through the ops and accumulate consecutive
    # :del ops, then once we get to an :ins, we traverse those :del
    # ops in the same order, looking for a matching one. If a :del
    # is matching, we flush an :upd and continue going thorugh ops.
    # Otherwise, we flush the non-matching :del as is and try the
    # next :del in the sequence. Finally, if there is no matching
    # :del, we flush :ins as is and continue going through the ops.
    do_annotate_updates(ops, [], [])
  end

  defp do_annotate_updates([{:del, key, item} | ops], pending_dels, acc) do
    do_annotate_updates(ops, [{:del, key, item} | pending_dels], acc)
  end

  defp do_annotate_updates([{:ins, key, item} | ops], pending_dels, acc) do
    find_matching_del(Enum.reverse(pending_dels), {:ins, key, item}, ops, acc)
  end

  defp do_annotate_updates([op | ops], pending_dels, acc) do
    do_annotate_updates(ops, [], [op | pending_dels ++ acc])
  end

  defp do_annotate_updates([], pending_dels, acc) do
    Enum.reverse(pending_dels ++ acc)
  end

  defp find_matching_del([{:del, key1, item1} | dels], {:ins, key2, item2}, ops, acc)
       when elem(key1, 0) == elem(key2, 0) do
    do_annotate_updates(ops, Enum.reverse(dels), [{:upd, key1, key2, item1, item2} | acc])
  end

  defp find_matching_del([del | dels], ins, ops, acc) do
    find_matching_del(dels, ins, ops, [del | acc])
  end

  defp find_matching_del([], ins, ops, acc) do
    do_annotate_updates(ops, [], [ins | acc])
  end

  defp diff_to_data_ops(diff, data, client_id) do
    acc = %{
      cid: client_id,
      data: data,
      section_id: nil,
      cell_idx: 0,
      sections_to_delete: [],
      pending_move_del: %{},
      applied_move_ins: MapSet.new(),
      # Maps section ids from after_notebook to the corresponding section
      # ids within the current data. We use this to translate parent_id
      # references present in after_notebook.
      section_id_map: %{}
    }

    do_diff_to_data_ops(diff, acc)
  end

  defp do_diff_to_data_ops(
         [{:eq, _key, %Section{} = before_section, %Section{} = after_section} | diff],
         acc
       ) do
    section_id_map = Map.put(acc.section_id_map, after_section.id, before_section.id)
    acc = %{acc | section_id: before_section.id, cell_idx: 0, section_id_map: section_id_map}
    do_diff_to_data_ops(diff, acc)
  end

  defp do_diff_to_data_ops([{:eq, _key, before_cell, _after_cell} | diff], acc)
       when is_cell(before_cell) do
    acc = %{acc | cell_idx: acc.cell_idx + 1}
    do_diff_to_data_ops(diff, acc)
  end

  defp do_diff_to_data_ops([{:ins, _key, %Section{} = section} | diff], acc) do
    insert_op = {:insert_section_into, acc.cid, acc.section_id, acc.cell_idx, section.id}

    name_op =
      if section.name != Section.new().name do
        [{:set_section_name, acc.cid, section.id, section.name}]
      else
        []
      end

    parent_op =
      if section.parent_id do
        mapped_parent_id = Map.fetch!(acc.section_id_map, section.parent_id)
        [{:set_section_parent, acc.cid, section.id, mapped_parent_id}]
      else
        []
      end

    section_id_map = Map.put(acc.section_id_map, section.id, section.id)
    acc = %{acc | section_id: section.id, cell_idx: 0, section_id_map: section_id_map}
    [insert_op | name_op ++ parent_op ++ do_diff_to_data_ops(diff, acc)]
  end

  defp do_diff_to_data_ops([{:ins, _key, cell} | diff], acc) when is_cell(cell) do
    type = Cell.type(cell)
    attrs = attrs_diff(Cell.new(type), cell, [:source | cell_syncable_fields(cell)])
    op = {:insert_cell, acc.cid, acc.section_id, acc.cell_idx, type, cell.id, attrs}
    acc = %{acc | cell_idx: acc.cell_idx + 1}
    [op | do_diff_to_data_ops(diff, acc)]
  end

  defp do_diff_to_data_ops([{:del, _key, %Section{} = before_section} | diff], acc) do
    # Deleting the section at this point may cause issues for subsequent
    # operations. For example, if this is the leading section, the
    # cells need to be removed first, and only then the section can
    # be deleted. Similarly, if there are child sections, those need
    # to unset the parent first. To avoid such issues, we keep the
    # section as is, we mark it for deletion, and at the very end we
    # add the section deletion operation.

    acc = %{
      acc
      | sections_to_delete: [before_section.id | acc.sections_to_delete],
        section_id: before_section.id,
        cell_idx: 0
    }

    do_diff_to_data_ops(diff, acc)
  end

  defp do_diff_to_data_ops([{:del, _key, cell} | diff], acc) when is_cell(cell) do
    op = {:delete_cell, acc.cid, cell.id}
    [op | do_diff_to_data_ops(diff, acc)]
  end

  defp do_diff_to_data_ops(
         [{:upd, _key1, _key2, %Section{} = before_section, %Section{} = after_section} | diff],
         acc
       ) do
    name_op =
      if after_section.name != before_section.name do
        [{:set_section_name, acc.cid, before_section.id, after_section.name}]
      else
        []
      end

    mapped_parent_id =
      if after_section.parent_id,
        do: Map.fetch!(acc.section_id_map, after_section.parent_id),
        else: nil

    parent_op =
      cond do
        mapped_parent_id == before_section.parent_id ->
          []

        mapped_parent_id == nil ->
          [{:unset_section_parent, acc.cid, before_section.id}]

        true ->
          [{:set_section_parent, acc.cid, before_section.id, mapped_parent_id}]
      end

    section_id_map = Map.put(acc.section_id_map, after_section.id, before_section.id)
    acc = %{acc | section_id: before_section.id, cell_idx: 0, section_id_map: section_id_map}
    parent_op ++ name_op ++ do_diff_to_data_ops(diff, acc)
  end

  defp do_diff_to_data_ops([{:upd, _key1, _key2, before_cell, after_cell} | diff], acc)
       when is_cell(before_cell) and is_cell(after_cell) do
    ops =
      case before_cell do
        %Cell.Markdown{} ->
          [cell_delta_operation(before_cell, after_cell, acc.data, acc.cid)]

        %Cell.Code{} ->
          source_op =
            if before_cell.source != after_cell.source do
              [cell_delta_operation(before_cell, after_cell, acc.data, acc.cid)]
            else
              []
            end

          updated_attrs = attrs_diff(before_cell, after_cell, cell_syncable_fields(before_cell))

          attrs_op =
            if updated_attrs != %{} do
              [{:set_cell_attributes, acc.cid, before_cell.id, updated_attrs}]
            else
              []
            end

          source_op ++ attrs_op

        %Cell.Smart{} ->
          # Smart cells are controlled by Livebook, we only sync with
          # common cell-related metadata, anything else must be ignored.
          updated_attrs = attrs_diff(before_cell, after_cell, cell_syncable_fields(before_cell))

          if updated_attrs != %{} do
            [{:set_cell_attributes, acc.cid, before_cell.id, updated_attrs}]
          else
            []
          end
      end

    acc = %{acc | cell_idx: acc.cell_idx + 1}

    ops ++ do_diff_to_data_ops(diff, acc)
  end

  defp do_diff_to_data_ops([{:move_ins, _key, before_cell} | diff], acc)
       when is_cell(before_cell) do
    op = {:move_cell, acc.cid, before_cell.id, acc.section_id, acc.cell_idx}

    {from_section_id, pending_move_del} = Map.pop(acc.pending_move_del, before_cell.id)

    cell_idx =
      if from_section_id != nil and from_section_id == acc.section_id do
        # There was a :move_del in the same section and we kept the
        # cell in place, so after the move the section has the same
        # number of cells, and therefore we don't bump cell_idx.
        acc.cell_idx
      else
        acc.cell_idx + 1
      end

    acc = %{
      acc
      | applied_move_ins: MapSet.put(acc.applied_move_ins, before_cell.id),
        pending_move_del: pending_move_del,
        cell_idx: cell_idx
    }

    [op | do_diff_to_data_ops(diff, acc)]
  end

  defp do_diff_to_data_ops([{:move_del, _key, before_cell} | diff], acc)
       when is_cell(before_cell) do
    if before_cell.id in acc.applied_move_ins do
      # :move_ins already seen, so the cell has already been moved up
      # and we can ignore this :move_del.
      do_diff_to_data_ops(diff, acc)
    else
      # :move_ins not seen yet, so we keep the cell in place for now
      # and it will be moved down later.
      pending_move_del = Map.put(acc.pending_move_del, before_cell.id, acc.section_id)
      acc = %{acc | pending_move_del: pending_move_del, cell_idx: acc.cell_idx + 1}
      do_diff_to_data_ops(diff, acc)
    end
  end

  defp do_diff_to_data_ops([], acc) do
    # Final operations.
    for section_id <- acc.sections_to_delete do
      {:delete_section, acc.cid, section_id, _delete_cells = false}
    end
  end

  defp cell_delta_operation(before_cell, after_cell, data, client_id) do
    delta = Livebook.Text.Delta.diff(before_cell.source, after_cell.source)
    revision = data.cell_infos[before_cell.id].sources.primary.revision
    {:apply_cell_delta, client_id, before_cell.id, :primary, delta, nil, revision}
  end
end
