defmodule Livebook.Session.Data do
  @moduledoc false

  # A structure with shared session data.
  #
  # In some sense this structure is a `Notebook` decorated
  # with all the ephemeral session data.
  #
  # The data is kept both in the `Session` process and all the client processes.
  # All changes go to the `Session` process first to introduce linearity
  # and then are broadcasted to the clients, hence every client
  # receives changes in the same order.
  # Upon receiving a change message, every process applies
  # the change to the locally stored `Data`. In this way the local `Data`
  # stays the same in all processes, while the messages are minimal.

  defstruct [
    :notebook,
    :origin,
    :file,
    :dirty,
    :section_infos,
    :cell_infos,
    :input_values,
    :bin_entries,
    :runtime,
    :clients_map,
    :users_map
  ]

  alias Livebook.{Notebook, Delta, Runtime, JSInterop, FileSystem}
  alias Livebook.Users.User
  alias Livebook.Notebook.{Cell, Section}
  alias Livebook.Utils.Graph

  @type t :: %__MODULE__{
          notebook: Notebook.t(),
          origin: String.t() | nil,
          file: FileSystem.File.t() | nil,
          dirty: boolean(),
          section_infos: %{Section.id() => section_info()},
          cell_infos: %{Cell.id() => cell_info()},
          input_values: %{input_id() => term()},
          bin_entries: list(cell_bin_entry()),
          runtime: Runtime.t() | nil,
          clients_map: %{pid() => User.id()},
          users_map: %{User.id() => User.t()}
        }

  @type section_info :: %{
          evaluating_cell_id: Cell.id(),
          evaluation_queue: list(Cell.id())
        }

  @type cell_info :: %{
          validity_status: cell_validity_status(),
          evaluation_status: cell_evaluation_status(),
          revision: cell_revision(),
          deltas: list(Delta.t()),
          revision_by_client_pid: %{pid() => cell_revision()},
          snapshot: snapshot(),
          evaluation_digest: String.t() | nil,
          evaluation_snapshot: snapshot() | nil,
          evaluation_time_ms: integer() | nil,
          evaluation_start: DateTime.t() | nil,
          evaluation_number: non_neg_integer(),
          outputs_batch_number: non_neg_integer(),
          bound_to_input_ids: MapSet.t(input_id()),
          bound_input_readings: input_reading()
        }

  @type cell_bin_entry :: %{
          cell: Cell.t(),
          section_id: Section.id(),
          section_name: String.t(),
          index: non_neg_integer(),
          deleted_at: DateTime.t()
        }

  @type cell_revision :: non_neg_integer()

  @type cell_validity_status :: :fresh | :evaluated | :stale | :aborted
  @type cell_evaluation_status :: :ready | :queued | :evaluating

  @type input_id :: String.t()

  @type client :: {User.id(), pid()}

  @type index :: non_neg_integer()

  # Snapshot holds information about the cell evaluation dependencies,
  # for example what's the previous cell, the number of times that
  # cell was evaluated, the list of available inputs, etc.
  # Whenever the snapshot changes, it implies a new evaluation context,
  # and basically means the cell got stale.
  #
  # The snapshot comprises of two actual snapshots:
  #
  #   * `deps_snapshot` - everything related to parent cells and their
  #     evaluations. This is recorded once the cell starts evaluating
  #
  #   * `bound_inputs_snapshot` - snapshot of the inputs and their values
  #     used by cell evaluation. This is recorded once the cell finishes
  #     its evaluation
  #
  @type snapshot :: {deps_snapshot :: term(), bound_inputs_snapshot :: term()}

  @type input_reading :: {input_id(), input_value :: term()}

  # Note that all operations carry the pid of whatever
  # process originated the operation. Some operations
  # like :apply_cell_delta and :report_cell_revision
  # require the pid to be a registered client, as in these
  # cases it's necessary for the operation to be properly applied.
  # For other operations the pid can represent an arbitrary process
  # and is passed for informative purposes only.

  @type operation ::
          {:set_notebook_attributes, pid(), map()}
          | {:insert_section, pid(), index(), Section.id()}
          | {:insert_section_into, pid(), Section.id(), index(), Section.id()}
          | {:set_section_parent, pid(), Section.id(), parent_id :: Section.id()}
          | {:unset_section_parent, pid(), Section.id()}
          | {:insert_cell, pid(), Section.id(), index(), Cell.type(), Cell.id()}
          | {:delete_section, pid(), Section.id(), delete_cells :: boolean()}
          | {:delete_cell, pid(), Cell.id()}
          | {:restore_cell, pid(), Cell.id()}
          | {:move_cell, pid(), Cell.id(), offset :: integer()}
          | {:move_section, pid(), Section.id(), offset :: integer()}
          | {:queue_cells_evaluation, pid(), list(Cell.id())}
          | {:evaluation_started, pid(), Cell.id(), binary()}
          | {:add_cell_evaluation_output, pid(), Cell.id(), term()}
          | {:add_cell_evaluation_response, pid(), Cell.id(), term(), metadata :: map()}
          | {:bind_input, pid(), elixir_cell_id :: Cell.id(), input_id()}
          | {:reflect_main_evaluation_failure, pid()}
          | {:reflect_evaluation_failure, pid(), Section.id()}
          | {:cancel_cell_evaluation, pid(), Cell.id()}
          | {:erase_outputs, pid()}
          | {:set_notebook_name, pid(), String.t()}
          | {:set_section_name, pid(), Section.id(), String.t()}
          | {:client_join, pid(), User.t()}
          | {:client_leave, pid()}
          | {:update_user, pid(), User.t()}
          | {:apply_cell_delta, pid(), Cell.id(), Delta.t(), cell_revision()}
          | {:report_cell_revision, pid(), Cell.id(), cell_revision()}
          | {:set_cell_attributes, pid(), Cell.id(), map()}
          | {:set_input_value, pid(), input_id(), value :: term()}
          | {:set_runtime, pid(), Runtime.t() | nil}
          | {:set_file, pid(), FileSystem.File.t() | nil}
          | {:set_autosave_interval, pid(), non_neg_integer() | nil}
          | {:mark_as_not_dirty, pid()}

  @type action ::
          :start_runtime
          | {:start_evaluation, Cell.t(), Section.t()}
          | {:stop_evaluation, Section.t()}
          | {:forget_evaluation, Cell.t(), Section.t()}
          | {:broadcast_delta, pid(), Cell.t(), Delta.t()}

  @doc """
  Returns a fresh notebook session state.
  """
  @spec new(Notebook.t()) :: t()
  def new(notebook \\ Notebook.new()) do
    data = %__MODULE__{
      notebook: notebook,
      origin: nil,
      file: nil,
      dirty: true,
      section_infos: initial_section_infos(notebook),
      cell_infos: initial_cell_infos(notebook),
      input_values: initial_input_values(notebook),
      bin_entries: [],
      runtime: nil,
      clients_map: %{},
      users_map: %{}
    }

    data
    |> with_actions()
    |> compute_snapshots()
    |> elem(0)
  end

  defp initial_section_infos(notebook) do
    for section <- notebook.sections,
        into: %{},
        do: {section.id, new_section_info()}
  end

  defp initial_cell_infos(notebook) do
    for section <- notebook.sections,
        cell <- section.cells,
        into: %{},
        do: {cell.id, new_cell_info(%{})}
  end

  defp initial_input_values(notebook) do
    for section <- notebook.sections,
        %Cell.Elixir{} = cell <- section.cells,
        output <- cell.outputs,
        attrs <- Cell.Elixir.find_inputs_in_output(output),
        into: %{},
        do: {attrs.id, attrs.default}
  end

  @doc """
  Applies the change specified by `operation` to the given session `data`.

  All operations are reproducible (i.e. this function is pure),
  so provided all processes have the same session data
  they can individually apply any given operation and end up in the same state.

  An operation only applies changes to the structure, but it doesn't trigger
  any actual processing. It's the responsibility of the session process to ensure
  the system reflects the new structure. For instance, when a new cell is marked
  as evaluating, the session process should take care of triggering actual evaluation.

  Returns `{:ok, data, actions}` if the operation is valid, where `data` is the result
  of applying said operation to the given data, and `actions` is a list
  of side effects that should be performed for the new data to hold true.

  Returns `:error` if the operation is not valid. The `:error` is generally
  expected given the collaborative nature of sessions. For example if there are
  simultaneous deletion and evaluation operations on the same cell, we may perform delete first,
  in which case the evaluation is no longer valid (there's no cell with the given id).
  By returning `:error` we simply notify the caller that no changes were applied,
  so any related actions can be ignored.
  """
  @spec apply_operation(t(), operation()) :: {:ok, t(), list(action())} | :error
  def apply_operation(data, operation)

  def apply_operation(data, {:set_notebook_attributes, _client_pid, attrs}) do
    with true <- valid_attrs_for?(data.notebook, attrs) do
      data
      |> with_actions()
      |> set_notebook_attributes(attrs)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:insert_section, _client_pid, index, id}) do
    section = %{Section.new() | id: id}

    data
    |> with_actions()
    |> insert_section(index, section)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:insert_section_into, _client_pid, section_id, index, id}) do
    with {:ok, _section} <- Notebook.fetch_section(data.notebook, section_id) do
      section = %{Section.new() | id: id}

      data
      |> with_actions()
      |> insert_section_into(section_id, index, section)
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:set_section_parent, _client_pid, section_id, parent_id}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, section_id),
         {:ok, parent_section} <- Notebook.fetch_section(data.notebook, parent_id),
         true <- section.parent_id != parent_id,
         [] <- Notebook.child_sections(data.notebook, section.id),
         true <- parent_section in Notebook.valid_parents_for(data.notebook, section.id) do
      data
      |> with_actions()
      |> cancel_section_evaluation(section)
      |> set_section_parent(section, parent_section)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:unset_section_parent, _client_pid, section_id}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, section_id),
         true <- section.parent_id != nil do
      data
      |> with_actions()
      |> cancel_section_evaluation(section)
      |> add_action({:stop_evaluation, section})
      |> unset_section_parent(section)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:insert_cell, _client_pid, section_id, index, type, id}) do
    with {:ok, _section} <- Notebook.fetch_section(data.notebook, section_id) do
      cell = %{Cell.new(type) | id: id}

      data
      |> with_actions()
      |> insert_cell(section_id, index, cell)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:delete_section, _client_pid, id, delete_cells}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, id),
         true <- section != hd(data.notebook.sections) or delete_cells,
         [] <- Notebook.child_sections(data.notebook, section.id) do
      data
      |> with_actions()
      |> delete_section(section, delete_cells)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:delete_cell, _client_pid, id}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id) do
      data
      |> with_actions()
      |> delete_cell(cell, section)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:restore_cell, _client_pid, id}) do
    with {:ok, cell_bin_entry} <- fetch_cell_bin_entry(data, id),
         true <- data.notebook.sections != [] do
      data
      |> with_actions()
      |> restore_cell(cell_bin_entry)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:move_cell, _client_pid, id, offset}) do
    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         true <- offset != 0 do
      data
      |> with_actions()
      |> move_cell(cell, offset)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:move_section, _client_pid, id, offset}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, id),
         true <- offset != 0,
         true <- Notebook.can_move_section_by?(data.notebook, section, offset) do
      data
      |> with_actions()
      |> move_section(section, offset)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:queue_cells_evaluation, _client_pid, cell_ids}) do
    cells_with_section =
      data.notebook
      |> Notebook.elixir_cells_with_section()
      |> Enum.filter(fn {cell, _section} ->
        info = data.cell_infos[cell.id]
        cell.id in cell_ids and info.evaluation_status == :ready
      end)

    if cell_ids != [] and length(cell_ids) == length(cells_with_section) do
      cells_with_section
      |> Enum.reduce(with_actions(data), fn {cell, section}, data_actions ->
        data_actions
        |> queue_prerequisite_cells_evaluation(cell)
        |> queue_cell_evaluation(cell, section)
      end)
      |> maybe_start_runtime(data)
      |> maybe_evaluate_queued()
      |> compute_snapshots_and_validity()
      |> wrap_ok()
    else
      :error
    end
  end

  def apply_operation(data, {:evaluation_started, _client_pid, id, evaluation_digest}) do
    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         %Cell.Elixir{} <- cell,
         :evaluating <- data.cell_infos[cell.id].evaluation_status do
      data
      |> with_actions()
      |> update_cell_info!(cell.id, &%{&1 | evaluation_digest: evaluation_digest})
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:add_cell_evaluation_output, _client_pid, id, output}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, id) do
      data
      |> with_actions()
      |> add_cell_evaluation_output(cell, output)
      |> garbage_collect_input_values()
      |> mark_dirty_if_persisting_outputs()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:add_cell_evaluation_response, _client_pid, id, output, metadata}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         :evaluating <- data.cell_infos[cell.id].evaluation_status do
      data
      |> with_actions()
      |> add_cell_evaluation_response(cell, output)
      |> finish_cell_evaluation(cell, section, metadata)
      |> garbage_collect_input_values()
      |> compute_snapshots_and_validity()
      |> maybe_evaluate_queued()
      |> compute_snapshots_and_validity()
      |> mark_dirty_if_persisting_outputs()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:bind_input, _client_pid, cell_id, input_id}) do
    with {:ok, %Cell.Elixir{} = cell, _section} <-
           Notebook.fetch_cell_and_section(data.notebook, cell_id),
         true <- Map.has_key?(data.input_values, input_id),
         false <- MapSet.member?(data.cell_infos[cell.id].bound_to_input_ids, input_id) do
      data
      |> with_actions()
      |> bind_input(cell, input_id)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:reflect_main_evaluation_failure, _client_pid}) do
    data
    |> with_actions()
    |> clear_main_evaluation()
    |> wrap_ok()
  end

  def apply_operation(data, {:reflect_evaluation_failure, _client_pid, section_id}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, section_id) do
      data
      |> with_actions()
      |> clear_section_evaluation(section)
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:cancel_cell_evaluation, _client_pid, id}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         true <- data.cell_infos[cell.id].evaluation_status in [:evaluating, :queued] do
      data
      |> with_actions()
      |> cancel_cell_evaluation(cell, section)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:erase_outputs, _client_pid}) do
    data
    |> with_actions()
    |> erase_outputs()
    |> garbage_collect_input_values()
    |> wrap_ok()
  end

  def apply_operation(data, {:set_notebook_name, _client_pid, name}) do
    data
    |> with_actions()
    |> set_notebook_name(name)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:set_section_name, _client_pid, section_id, name}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, section_id) do
      data
      |> with_actions()
      |> set_section_name(section, name)
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:client_join, client_pid, user}) do
    with false <- Map.has_key?(data.clients_map, client_pid) do
      data
      |> with_actions()
      |> client_join(client_pid, user)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:client_leave, client_pid}) do
    with true <- Map.has_key?(data.clients_map, client_pid) do
      data
      |> with_actions()
      |> client_leave(client_pid)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:update_user, _client_pid, user}) do
    with true <- Map.has_key?(data.users_map, user.id) do
      data
      |> with_actions()
      |> update_user(user)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:apply_cell_delta, client_pid, cell_id, delta, revision}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         cell_info <- data.cell_infos[cell.id],
         true <- 0 < revision and revision <= cell_info.revision + 1,
         true <- Map.has_key?(data.clients_map, client_pid) do
      data
      |> with_actions()
      |> apply_delta(client_pid, cell, delta, revision)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:report_cell_revision, client_pid, cell_id, revision}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         cell_info <- data.cell_infos[cell.id],
         true <- 0 < revision and revision <= cell_info.revision,
         true <- Map.has_key?(data.clients_map, client_pid) do
      data
      |> with_actions()
      |> report_revision(client_pid, cell, revision)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:set_cell_attributes, _client_pid, cell_id, attrs}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         true <- valid_attrs_for?(cell, attrs) do
      data
      |> with_actions()
      |> set_cell_attributes(cell, attrs)
      |> compute_snapshots_and_validity()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:set_input_value, _client_pid, input_id, value}) do
    with true <- Map.has_key?(data.input_values, input_id) do
      data
      |> with_actions()
      |> set_input_value(input_id, value)
      |> compute_snapshots_and_validity()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:set_runtime, _client_pid, runtime}) do
    data
    |> with_actions()
    |> set_runtime(data, runtime)
    |> wrap_ok()
  end

  def apply_operation(data, {:set_file, _client_pid, file}) do
    data
    |> with_actions()
    |> set!(file: file)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:mark_as_not_dirty, _client_pid}) do
    data
    |> with_actions()
    |> set_dirty(false)
    |> wrap_ok()
  end

  # ===

  defp with_actions(data, actions \\ []), do: {data, actions}

  defp wrap_ok({data, actions}), do: {:ok, data, actions}

  defp set_notebook_attributes({data, _} = data_actions, attrs) do
    data_actions
    |> set!(notebook: Map.merge(data.notebook, attrs))
  end

  defp insert_section({data, _} = data_actions, index, section) do
    data_actions
    |> set!(
      notebook: Notebook.insert_section(data.notebook, index, section),
      section_infos: Map.put(data.section_infos, section.id, new_section_info())
    )
  end

  defp insert_section_into({data, _} = data_actions, section_id, index, section) do
    data_actions
    |> set!(
      notebook: Notebook.insert_section_into(data.notebook, section_id, index, section),
      section_infos: Map.put(data.section_infos, section.id, new_section_info())
    )
  end

  defp set_section_parent({data, _} = data_actions, section, parent_section) do
    data_actions
    |> set!(
      notebook:
        Notebook.update_section(data.notebook, section.id, fn section ->
          %{section | parent_id: parent_section.id}
        end)
    )
  end

  defp unset_section_parent({data, _} = data_actions, section) do
    data_actions
    |> set!(
      notebook:
        Notebook.update_section(data.notebook, section.id, fn section ->
          %{section | parent_id: nil}
        end)
    )
  end

  defp insert_cell({data, _} = data_actions, section_id, index, cell) do
    data_actions
    |> set!(
      notebook: Notebook.insert_cell(data.notebook, section_id, index, cell),
      cell_infos: Map.put(data.cell_infos, cell.id, new_cell_info(data.clients_map))
    )
  end

  defp delete_section(data_actions, section, delete_cells) do
    {data, _} =
      data_actions =
      if delete_cells do
        data_actions
        |> reduce(Enum.reverse(section.cells), &delete_cell(&1, &2, section))
      else
        data_actions
      end

    data_actions
    |> set!(
      notebook: Notebook.delete_section(data.notebook, section.id),
      section_infos: Map.delete(data.section_infos, section.id)
    )
  end

  defp delete_cell({data, _} = data_actions, cell, section) do
    data_actions
    |> cancel_cell_evaluation(cell, section)
    |> add_action({:forget_evaluation, cell, section})
    |> set!(
      notebook: Notebook.delete_cell(data.notebook, cell.id),
      bin_entries: [
        %{
          cell: cell,
          section_id: section.id,
          section_name: section.name,
          index: Enum.find_index(section.cells, &(&1 == cell)),
          deleted_at: DateTime.utc_now()
        }
        | data.bin_entries
      ]
    )
    |> delete_cell_info(cell)
  end

  defp delete_cell_info({data, _} = data_actions, cell) do
    data_actions
    |> set!(cell_infos: Map.delete(data.cell_infos, cell.id))
  end

  defp restore_cell({data, _} = data_actions, cell_bin_entry) do
    {section, index} =
      case Notebook.fetch_section(data.notebook, cell_bin_entry.section_id) do
        # Insert at the index of deletion, it may be no longer accurate,
        # but even then makes for a good approximation and the cell can be easily moved
        {:ok, section} -> {section, cell_bin_entry.index}
        # Insert at the end of the notebook if the section no longer exists
        :error -> {List.last(data.notebook.sections), -1}
      end

    data_actions
    |> insert_cell(section.id, index, cell_bin_entry.cell)
    |> set!(bin_entries: List.delete(data.bin_entries, cell_bin_entry))
  end

  defp move_cell({data, _} = data_actions, cell, offset) do
    updated_notebook = Notebook.move_cell(data.notebook, cell.id, offset)

    data_actions
    |> set!(notebook: updated_notebook)
    |> unqueue_cells_after_moved(data.notebook)
  end

  defp move_section({data, _} = data_actions, section, offset) do
    updated_notebook = Notebook.move_section(data.notebook, section.id, offset)

    data_actions
    |> set!(notebook: updated_notebook)
    |> unqueue_cells_after_moved(data.notebook)
  end

  defp unqueue_cells_after_moved({data, _} = data_actions, prev_notebook) do
    relevant_cell? = fn cell -> is_struct(cell, Cell.Elixir) end
    graph_before = Notebook.cell_dependency_graph(prev_notebook, cell_filter: relevant_cell?)
    graph_after = Notebook.cell_dependency_graph(data.notebook, cell_filter: relevant_cell?)

    # For each path in the dependency graph, find the upmost cell
    # which parent changed. From that point downwards all cells
    # are invalidated. Then gather invalidated cells from all paths
    # and unqueue them.

    invalidted_cell_ids =
      graph_after
      |> Graph.leaves()
      |> Enum.reduce(MapSet.new(), fn cell_id, invalidated ->
        invalidated_on_path(cell_id, graph_after, graph_before, [], [])
        |> MapSet.new()
        |> MapSet.union(invalidated)
      end)

    invalidated_cells_with_section =
      data.notebook
      |> Notebook.elixir_cells_with_section()
      |> Enum.filter(fn {cell, _} ->
        MapSet.member?(invalidted_cell_ids, cell.id)
      end)

    data_actions
    |> unqueue_cells_evaluation(invalidated_cells_with_section)
  end

  # Traverses path buttom-up looking for the upmost edge with changed parent.
  defp invalidated_on_path(child_id, graph_after, graph_before, visited, invalidated)

  defp invalidated_on_path(nil, _graph_after, _graph_before, _visited, invalidated),
    do: invalidated

  defp invalidated_on_path(child_id, graph_after, graph_before, visited, invalidated) do
    if graph_after[child_id] == graph_before[child_id] do
      invalidated_on_path(
        graph_after[child_id],
        graph_after,
        graph_before,
        [child_id | visited],
        invalidated
      )
    else
      invalidated_on_path(
        graph_after[child_id],
        graph_after,
        graph_before,
        [child_id | visited],
        [child_id | visited]
      )
    end
  end

  defp queue_cell_evaluation(data_actions, cell, section) do
    data_actions
    |> update_section_info!(section.id, fn section ->
      %{section | evaluation_queue: append_new(section.evaluation_queue, cell.id)}
    end)
    |> update_cell_info!(cell.id, fn info ->
      update_in(info.evaluation_status, fn
        :ready -> :queued
        other -> other
      end)
    end)
  end

  defp unqueue_cell_evaluation(data_actions, cell, section) do
    data_actions
    |> update_section_info!(section.id, fn section ->
      %{section | evaluation_queue: List.delete(section.evaluation_queue, cell.id)}
    end)
    |> set_cell_info!(cell.id, evaluation_status: :ready)
  end

  defp add_cell_evaluation_output(data_actions, cell, output) do
    data_actions
    |> add_cell_output(cell, output)
  end

  defp add_cell_evaluation_response(data_actions, cell, output) do
    data_actions
    |> add_cell_output(cell, output)
  end

  defp add_cell_output({data, _} = data_actions, cell, output) do
    data_actions
    |> set!(
      notebook: Notebook.add_cell_output(data.notebook, cell.id, output),
      input_values:
        {0, output}
        |> Cell.Elixir.find_inputs_in_output()
        |> Map.new(fn attrs -> {attrs.id, attrs.default} end)
        |> Map.merge(data.input_values)
    )
  end

  defp finish_cell_evaluation(data_actions, cell, section, metadata) do
    data_actions
    |> update_cell_info!(cell.id, fn info ->
      %{
        info
        | evaluation_status: :ready,
          evaluation_time_ms: metadata.evaluation_time_ms,
          # After finished evaluation, take the snapshot of read inputs
          evaluation_snapshot:
            {elem(info.evaluation_snapshot, 0),
             input_readings_snapshot(info.bound_input_readings)}
      }
    end)
    |> set_section_info!(section.id, evaluating_cell_id: nil)
  end

  defp maybe_start_runtime({data, _} = data_actions, prev_data) do
    if data.runtime == nil and not any_cell_queued?(prev_data) and any_cell_queued?(data) do
      add_action(data_actions, :start_runtime)
    else
      data_actions
    end
  end

  defp any_cell_queued?(data) do
    Enum.any?(data.section_infos, fn {_section_id, info} -> info.evaluation_queue != [] end)
  end

  # Don't trigger evaluation if we don't have a runtime started yet
  defp maybe_evaluate_queued({%{runtime: nil}, _} = data_actions), do: data_actions

  defp maybe_evaluate_queued({data, _} = data_actions) do
    main_flow_evaluating? = main_flow_evaluating?(data)

    {awaiting_branch_sections, awaiting_regular_sections} =
      data.notebook.sections
      |> Enum.filter(&section_awaits_evaluation?(data, &1.id))
      |> Enum.split_with(& &1.parent_id)

    data_actions =
      reduce(data_actions, awaiting_branch_sections, fn {data, _} = data_actions, section ->
        %{evaluation_queue: [id | _]} = data.section_infos[section.id]

        {:ok, parent} = Notebook.fetch_section(data.notebook, section.parent_id)

        prev_cell_section =
          data.notebook
          |> Notebook.parent_cells_with_section(id)
          |> Enum.find_value(parent, fn {cell, section} ->
            is_struct(cell, Cell.Elixir) && section
          end)

        prev_section_queued? =
          prev_cell_section != nil and
            data.section_infos[prev_cell_section.id].evaluation_queue != []

        # If evaluating this cell requires interaction with the main flow,
        # we keep the cell queued. In case of the Elixir runtimes the
        # evaluation context needs to be copied between evaluation processes
        # and this requires the main flow to be free of work.
        if prev_cell_section != section and (main_flow_evaluating? or prev_section_queued?) do
          data_actions
        else
          evaluate_next_cell_in_section(data_actions, section)
        end
      end)

    if awaiting_regular_sections != [] and not main_flow_evaluating? do
      section = hd(awaiting_regular_sections)
      evaluate_next_cell_in_section(data_actions, section)
    else
      data_actions
    end
  end

  defp main_flow_evaluating?(data) do
    Enum.any?(data.notebook.sections, fn section ->
      section.parent_id == nil and section_evaluating?(data, section.id)
    end)
  end

  defp section_evaluating?(data, section_id) do
    info = data.section_infos[section_id]
    info.evaluating_cell_id != nil
  end

  defp any_section_evaluating?(data) do
    Enum.any?(data.notebook.sections, fn section ->
      section_evaluating?(data, section.id)
    end)
  end

  defp section_awaits_evaluation?(data, section_id) do
    info = data.section_infos[section_id]
    info.evaluating_cell_id == nil and info.evaluation_queue != []
  end

  defp evaluate_next_cell_in_section({data, _} = data_actions, section) do
    case data.section_infos[section.id] do
      %{evaluating_cell_id: nil, evaluation_queue: [id | ids]} ->
        cell = Enum.find(section.cells, &(&1.id == id))

        data_actions
        |> set!(notebook: Notebook.update_cell(data.notebook, id, &%{&1 | outputs: []}))
        |> update_cell_info!(id, fn info ->
          %{
            info
            | # Note: we intentionally mark the cell as evaluating up front,
              # so that another queue operation doesn't cause duplicated
              # :start_evaluation action
              evaluation_status: :evaluating,
              evaluation_number: info.evaluation_number + 1,
              outputs_batch_number: info.outputs_batch_number + 1,
              evaluation_digest: nil,
              evaluation_snapshot: info.snapshot,
              bound_to_input_ids: MapSet.new(),
              bound_input_readings: [],
              # This is a rough estimate, the exact time is measured in the
              # evaluator itself
              evaluation_start: DateTime.utc_now()
          }
        end)
        |> set_section_info!(section.id, evaluating_cell_id: id, evaluation_queue: ids)
        |> add_action({:start_evaluation, cell, section})

      _ ->
        data_actions
    end
  end

  defp bind_input({data, _} = data_actions, cell, input_id) do
    data_actions
    |> update_cell_info!(cell.id, fn info ->
      %{
        info
        | bound_to_input_ids: MapSet.put(info.bound_to_input_ids, input_id),
          bound_input_readings: [
            {input_id, data.input_values[input_id]} | info.bound_input_readings
          ]
      }
    end)
  end

  defp clear_all_evaluation({data, _} = data_actions) do
    data_actions
    |> reduce(data.notebook.sections, &clear_section_evaluation/2)
  end

  defp clear_main_evaluation({data, _} = data_actions) do
    regular_sections = Enum.filter(data.notebook.sections, &(&1.parent_id == nil))

    data_actions
    |> reduce(regular_sections, &clear_section_evaluation/2)
  end

  defp clear_section_evaluation(data_actions, section) do
    data_actions
    |> set_section_info!(section.id, evaluating_cell_id: nil, evaluation_queue: [])
    |> reduce(
      section.cells,
      &update_cell_info!(&1, &2.id, fn info ->
        %{
          info
          | validity_status:
              if info.validity_status == :fresh and info.evaluation_status != :evaluating do
                :fresh
              else
                :aborted
              end,
            evaluation_status: :ready,
            evaluation_digest: nil,
            evaluation_snapshot: nil
        }
      end)
    )
  end

  defp queue_prerequisite_cells_evaluation({data, _} = data_actions, cell) do
    prerequisites_queue =
      data.notebook
      |> Notebook.parent_cells_with_section(cell.id)
      |> Enum.filter(fn {cell, _} -> is_struct(cell, Cell.Elixir) end)
      |> Enum.take_while(fn {parent_cell, _section} ->
        info = data.cell_infos[parent_cell.id]
        info.validity_status != :evaluated and info.evaluation_status == :ready
      end)
      |> Enum.reverse()

    data_actions
    |> reduce(prerequisites_queue, fn data_actions, {cell, section} ->
      queue_cell_evaluation(data_actions, cell, section)
    end)
  end

  defp cancel_cell_evaluation({data, _} = data_actions, cell, section) do
    case data.cell_infos[cell.id].evaluation_status do
      :evaluating ->
        data_actions
        |> then(fn data_actions ->
          if section.parent_id do
            clear_section_evaluation(data_actions, section)
          else
            clear_main_evaluation(data_actions)
          end
        end)
        |> add_action({:stop_evaluation, section})

      :queued ->
        data_actions
        |> unqueue_cell_evaluation(cell, section)
        |> unqueue_dependent_cells_evaluation(cell)

      _ ->
        data_actions
    end
  end

  defp unqueue_dependent_cells_evaluation({data, _} = data_actions, cell) do
    dependent = dependent_cells_with_section(data, cell.id)
    unqueue_cells_evaluation(data_actions, dependent)
  end

  defp unqueue_cells_evaluation({data, _} = data_actions, cells_with_section) do
    queued_cells_with_section =
      Enum.filter(cells_with_section, fn {cell, _} ->
        data.cell_infos[cell.id].evaluation_status == :queued
      end)

    data_actions
    |> reduce(queued_cells_with_section, fn data_actions, {cell, section} ->
      unqueue_cell_evaluation(data_actions, cell, section)
    end)
  end

  defp cancel_section_evaluation({data, _} = data_actions, section) do
    case data.section_infos[section.id] do
      %{evaluating_cell_id: nil} ->
        data_actions

      %{evaluating_cell_id: evaluating_cell_id} ->
        cell = Enum.find(section.cells, &(&1.id == evaluating_cell_id))
        cancel_cell_evaluation(data_actions, cell, section)
    end
  end

  defp erase_outputs({data, _} = data_actions) do
    data_actions
    |> clear_all_evaluation()
    |> set!(
      notebook:
        Notebook.update_cells(data.notebook, fn
          %Cell.Elixir{} = cell -> %{cell | outputs: []}
          cell -> cell
        end)
    )
    |> update_every_cell_info(fn info ->
      %{info | outputs_batch_number: info.outputs_batch_number + 1}
    end)
  end

  defp set_notebook_name({data, _} = data_actions, name) do
    data_actions
    |> set!(notebook: %{data.notebook | name: name})
  end

  defp set_section_name({data, _} = data_actions, section, name) do
    data_actions
    |> set!(notebook: Notebook.update_section(data.notebook, section.id, &%{&1 | name: name}))
  end

  defp client_join({data, _} = data_actions, client_pid, user) do
    data_actions
    |> set!(
      clients_map: Map.put(data.clients_map, client_pid, user.id),
      users_map: Map.put(data.users_map, user.id, user)
    )
    |> update_every_cell_info(fn info ->
      put_in(info.revision_by_client_pid[client_pid], info.revision)
    end)
  end

  defp client_leave({data, _} = data_actions, client_pid) do
    {user_id, clients_map} = Map.pop(data.clients_map, client_pid)

    users_map =
      if user_id in Map.values(clients_map) do
        data.users_map
      else
        Map.delete(data.users_map, user_id)
      end

    data_actions
    |> set!(
      clients_map: clients_map,
      users_map: users_map
    )
    |> update_every_cell_info(fn info ->
      {_, info} = pop_in(info.revision_by_client_pid[client_pid])
      purge_deltas(info)
    end)
  end

  defp update_user({data, _} = data_actions, user) do
    set!(data_actions, users_map: Map.put(data.users_map, user.id, user))
  end

  defp apply_delta({data, _} = data_actions, client_pid, cell, delta, revision) do
    info = data.cell_infos[cell.id]

    deltas_ahead = Enum.take(info.deltas, -(info.revision - revision + 1))

    transformed_new_delta =
      Enum.reduce(deltas_ahead, delta, fn delta_ahead, transformed_new_delta ->
        Delta.transform(delta_ahead, transformed_new_delta, :left)
      end)

    # Note: the clients drop cell's source once it's no longer needed
    new_source =
      case cell.source do
        :__pruned__ -> :__pruned__
        source -> JSInterop.apply_delta_to_string(transformed_new_delta, source)
      end

    data_actions
    |> set!(notebook: Notebook.update_cell(data.notebook, cell.id, &%{&1 | source: new_source}))
    |> update_cell_info!(cell.id, fn info ->
      info = %{
        info
        | deltas: info.deltas ++ [transformed_new_delta],
          revision: info.revision + 1
      }

      # Before receiving acknowledgement, the client receives all the other deltas,
      # so we can assume they are in sync with the server and have the same revision.
      info = put_in(info.revision_by_client_pid[client_pid], info.revision)
      purge_deltas(info)
    end)
    |> add_action(
      {:broadcast_delta, client_pid, %{cell | source: new_source}, transformed_new_delta}
    )
  end

  defp report_revision(data_actions, client_pid, cell, revision) do
    data_actions
    |> update_cell_info!(cell.id, fn info ->
      info = put_in(info.revision_by_client_pid[client_pid], revision)
      purge_deltas(info)
    end)
  end

  defp set_cell_attributes({data, _} = data_actions, cell, attrs) do
    data_actions
    |> set!(notebook: Notebook.update_cell(data.notebook, cell.id, &Map.merge(&1, attrs)))
  end

  defp set_input_value({data, _} = data_actions, input_id, value) do
    data_actions
    |> set!(input_values: Map.put(data.input_values, input_id, value))
  end

  defp set_runtime(data_actions, prev_data, runtime) do
    {data, _} = data_actions = set!(data_actions, runtime: runtime)

    if prev_data.runtime == nil and data.runtime != nil do
      maybe_evaluate_queued(data_actions)
    else
      clear_all_evaluation(data_actions)
    end
  end

  defp purge_deltas(cell_info) do
    # Given client at revision X and upstream revision Y,
    # we need Y - X last deltas that the client is not aware of,
    # so that later we can use them to transform whatever
    # the client sends us as an update.
    #
    # We find the client that is the most behind and keep
    # as many deltas as we need for them.

    min_client_revision =
      cell_info.revision_by_client_pid
      |> Map.values()
      |> Enum.min(fn -> cell_info.revision end)

    necessary_deltas = cell_info.revision - min_client_revision
    deltas = Enum.take(cell_info.deltas, -necessary_deltas)

    %{cell_info | deltas: deltas}
  end

  defp fetch_cell_bin_entry(data, cell_id) do
    Enum.find_value(data.bin_entries, :error, fn entry ->
      entry.cell.id == cell_id && {:ok, entry}
    end)
  end

  defp add_action({data, actions}, action) do
    {data, actions ++ [action]}
  end

  defp append_new(list, item) do
    if item in list do
      list
    else
      list ++ [item]
    end
  end

  defp garbage_collect_input_values({data, _} = data_actions) do
    if any_section_evaluating?(data) do
      # Wait if evaluation is ongoing as it may render inputs
      data_actions
    else
      used_input_ids = data.notebook |> initial_input_values() |> Map.keys()
      set!(data_actions, input_values: Map.take(data.input_values, used_input_ids))
    end
  end

  defp new_section_info() do
    %{
      evaluating_cell_id: nil,
      evaluation_queue: []
    }
  end

  defp new_cell_info(clients_map) do
    client_pids = Map.keys(clients_map)

    %{
      revision: 0,
      deltas: [],
      revision_by_client_pid: Map.new(client_pids, &{&1, 0}),
      validity_status: :fresh,
      evaluation_status: :ready,
      evaluation_digest: nil,
      evaluation_time_ms: nil,
      evaluation_start: nil,
      evaluation_number: 0,
      outputs_batch_number: 0,
      bound_to_input_ids: MapSet.new(),
      bound_input_readings: [],
      snapshot: {nil, nil},
      evaluation_snapshot: nil
    }
  end

  defp set!({data, actions}, changes) do
    Enum.reduce(changes, data, fn {key, value}, info ->
      Map.replace!(info, key, value)
    end)
    |> with_actions(actions)
  end

  defp set_cell_info!(data_actions, cell_id, changes) do
    update_cell_info!(data_actions, cell_id, fn info ->
      Enum.reduce(changes, info, fn {key, value}, info ->
        Map.replace!(info, key, value)
      end)
    end)
  end

  defp update_cell_info!({data, _} = data_actions, cell_id, fun) do
    cell_infos = Map.update!(data.cell_infos, cell_id, fun)
    set!(data_actions, cell_infos: cell_infos)
  end

  defp update_every_cell_info({data, _} = data_actions, fun) do
    cell_infos =
      Map.new(data.cell_infos, fn {cell_id, info} ->
        {cell_id, fun.(info)}
      end)

    set!(data_actions, cell_infos: cell_infos)
  end

  defp set_section_info!(data_actions, section_id, changes) do
    update_section_info!(data_actions, section_id, fn info ->
      Enum.reduce(changes, info, fn {key, value}, info ->
        Map.replace!(info, key, value)
      end)
    end)
  end

  defp update_section_info!({data, _} = data_actions, section_id, fun) do
    section_infos = Map.update!(data.section_infos, section_id, fun)
    set!(data_actions, section_infos: section_infos)
  end

  defp reduce(data_actions, list, reducer) do
    Enum.reduce(list, data_actions, fn elem, data_actions -> reducer.(data_actions, elem) end)
  end

  defp set_dirty(data_actions, dirty \\ true) do
    set!(data_actions, dirty: dirty)
  end

  defp mark_dirty_if_persisting_outputs({%{notebook: %{persist_outputs: true}}, _} = data_actions) do
    set_dirty(data_actions)
  end

  defp mark_dirty_if_persisting_outputs(data_actions), do: data_actions

  defp valid_attrs_for?(struct, attrs) do
    Enum.all?(attrs, fn {key, _} -> Map.has_key?(struct, key) end)
  end

  @doc """
  Find cells bound to the given input.
  """
  @spec bound_cells_with_section(t(), input_id()) :: list({Cell.t(), Section.t()})
  def bound_cells_with_section(data, input_id) do
    data.notebook
    |> Notebook.cells_with_section()
    |> Enum.filter(fn {cell, _} ->
      info = data.cell_infos[cell.id]
      MapSet.member?(info.bound_to_input_ids, input_id)
    end)
  end

  defp dependent_cells_with_section(data, cell_id) do
    data.notebook
    |> Notebook.child_cells_with_section(cell_id)
    |> Enum.filter(fn {cell, _} -> is_struct(cell, Cell.Elixir) end)
  end

  # Computes cell snapshots and updates validity based on the new values.
  defp compute_snapshots_and_validity(data_actions) do
    data_actions
    |> compute_snapshots()
    |> update_validity()
    # After updating validity there may be new stale cells, so we check
    # if any of them is configured for automatic reevaluation
    |> maybe_queue_reevaluating_cells()
    |> maybe_evaluate_queued()
  end

  defp compute_snapshots({data, _} = data_actions) do
    graph =
      Notebook.cell_dependency_graph(data.notebook, cell_filter: &is_struct(&1, Cell.Elixir))

    cells_with_section = Notebook.elixir_cells_with_section(data.notebook)

    cell_snapshots =
      Enum.reduce(cells_with_section, %{}, fn {cell, section}, cell_snapshots ->
        info = data.cell_infos[cell.id]
        prev_cell_id = graph[cell.id]

        is_branch? = section.parent_id != nil

        parent_deps =
          prev_cell_id &&
            {
              prev_cell_id,
              cell_snapshots[prev_cell_id],
              number_of_evaluations(data.cell_infos[prev_cell_id])
            }

        deps = {is_branch?, parent_deps}
        deps_snapshot = :erlang.phash2(deps)

        inputs_snapshot =
          if info.evaluation_status == :evaluating do
            # While the cell is evaluating the bound inputs snapshot
            # is not stable, so we reuse the previous snapshot
            elem(info.snapshot, 1)
          else
            bound_inputs_snapshot(data, cell)
          end

        snapshot = {deps_snapshot, inputs_snapshot}
        put_in(cell_snapshots[cell.id], snapshot)
      end)

    reduce(data_actions, cells_with_section, fn data_actions, {cell, _} ->
      update_cell_info!(data_actions, cell.id, fn info ->
        snapshot = cell_snapshots[cell.id]
        %{info | snapshot: snapshot}
      end)
    end)
  end

  defp number_of_evaluations(%{evaluation_status: :evaluating} = info) do
    info.evaluation_number - 1
  end

  defp number_of_evaluations(info), do: info.evaluation_number

  defp bound_inputs_snapshot(data, cell) do
    %{bound_to_input_ids: bound_to_input_ids} = data.cell_infos[cell.id]

    for(
      input_id <- bound_to_input_ids,
      do: {input_id, data.input_values[input_id]}
    )
    |> input_readings_snapshot()
  end

  defp input_readings_snapshot([]), do: :empty

  defp input_readings_snapshot(name_value_pairs) do
    name_value_pairs |> Enum.sort() |> :erlang.phash2()
  end

  defp update_validity({data, _} = data_actions) do
    cells_with_section = Notebook.elixir_cells_with_section(data.notebook)

    reduce(data_actions, cells_with_section, fn data_actions, {cell, _} ->
      update_cell_info!(data_actions, cell.id, fn info ->
        validity_status =
          case info do
            %{evaluation_snapshot: snapshot, snapshot: snapshot} -> :evaluated
            %{evaluation_snapshot: nil, validity_status: :aborted} -> :aborted
            %{evaluation_snapshot: nil} -> :fresh
            _ -> :stale
          end

        %{info | validity_status: validity_status}
      end)
    end)
  end

  defp maybe_queue_reevaluating_cells({data, _} = data_actions) do
    cells_to_reeavaluete =
      data.notebook
      |> Notebook.elixir_cells_with_section()
      |> Enum.filter(fn {cell, _section} ->
        info = data.cell_infos[cell.id]
        info.validity_status == :stale and cell.reevaluate_automatically
      end)

    data_actions
    |> reduce(cells_to_reeavaluete, fn data_actions, {cell, section} ->
      data_actions
      |> queue_prerequisite_cells_evaluation(cell)
      |> queue_cell_evaluation(cell, section)
    end)
  end

  @doc """
  Checks if the given cell is outdated.

  A cell is considered outdated if its new/fresh or its content
  has changed since the last evaluation.
  """
  @spec cell_outdated?(t(), Cell.t()) :: boolean()
  def cell_outdated?(data, cell) do
    info = data.cell_infos[cell.id]
    digest = :erlang.md5(cell.source)
    info.validity_status != :evaluated or info.evaluation_digest != digest
  end

  @doc """
  Returns the list of cell ids for full evaluation.

  The list includes all outdated cells, cells in `forced_cell_ids`
  and all of their child cells.
  """
  @spec cell_ids_for_full_evaluation(t(), list(Cell.id())) :: list(Cell.id())
  def cell_ids_for_full_evaluation(data, forced_cell_ids) do
    elixir_cells_with_section = Notebook.elixir_cells_with_section(data.notebook)

    evaluable_cell_ids =
      for {cell, _} <- elixir_cells_with_section,
          cell_outdated?(data, cell) or cell.id in forced_cell_ids,
          uniq: true,
          do: cell.id

    cell_ids = Notebook.cell_ids_with_children(data.notebook, evaluable_cell_ids)

    for {cell, _} <- elixir_cells_with_section,
        cell.id in cell_ids,
        do: cell.id
  end
end
