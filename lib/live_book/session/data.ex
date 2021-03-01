defmodule LiveBook.Session.Data do
  @moduledoc false

  # A structure with shared session data.
  #
  # In some sense this structure is a `Notebook` decorated
  # with all the emphemeral session data.
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
    :path,
    :dirty,
    :section_infos,
    :cell_infos,
    :deleted_sections,
    :deleted_cells,
    :runtime,
    :client_pids
  ]

  alias LiveBook.{Notebook, Evaluator, Delta, Runtime, JSInterop}
  alias LiveBook.Notebook.{Cell, Section}

  @type t :: %__MODULE__{
          notebook: Notebook.t(),
          path: nil | String.t(),
          dirty: boolean(),
          section_infos: %{Section.id() => section_info()},
          cell_infos: %{Cell.id() => cell_info()},
          deleted_sections: list(Section.t()),
          deleted_cells: list(Cell.t()),
          runtime: Runtime.t() | nil,
          client_pids: list(pid())
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
          evaluated_at: DateTime.t()
        }

  @type cell_revision :: non_neg_integer()

  @type cell_validity_status :: :fresh | :evaluated | :stale
  @type cell_evaluation_status :: :ready | :queued | :evaluating

  @type index :: non_neg_integer()

  # Note that all operations carry the pid of whatever
  # process originated the operation. Some operations
  # like :apply_cell_delta and :report_cell_revision
  # require the pid to be a registered client, as in these
  # cases it's necessary for the operation to be properly applied.
  # For other operations the pid can represent an arbitrary process
  # and is passed for informative purposes only.

  @type operation ::
          {:insert_section, pid(), index(), Section.id()}
          | {:insert_cell, pid(), Section.id(), index(), Cell.type(), Cell.id()}
          | {:delete_section, pid(), Section.id()}
          | {:delete_cell, pid(), Cell.id()}
          | {:move_cell, pid(), Cell.id(), offset :: integer()}
          | {:queue_cell_evaluation, pid(), Cell.id()}
          | {:add_cell_evaluation_stdout, pid(), Cell.id(), String.t()}
          | {:add_cell_evaluation_response, pid(), Cell.id(), Evaluator.evaluation_response()}
          | {:cancel_cell_evaluation, pid(), Cell.id()}
          | {:set_notebook_name, pid(), String.t()}
          | {:set_section_name, pid(), Section.id(), String.t()}
          | {:client_join, pid()}
          | {:client_leave, pid()}
          | {:apply_cell_delta, pid(), Cell.id(), Delta.t(), cell_revision()}
          | {:report_cell_revision, pid(), Cell.id(), cell_revision()}
          | {:set_runtime, pid(), Runtime.t() | nil}
          | {:set_path, pid(), String.t() | nil}
          | {:mark_as_not_dirty, pid()}

  @type action ::
          {:start_evaluation, Cell.t(), Section.t()}
          | {:stop_evaluation, Section.t()}
          | {:forget_evaluation, Cell.t(), Section.t()}
          | {:broadcast_delta, pid(), Cell.t(), Delta.t()}

  @doc """
  Returns a fresh notebook session state.
  """
  @spec new(Notebook.t()) :: t()
  def new(notebook \\ Notebook.new()) do
    %__MODULE__{
      notebook: notebook,
      path: nil,
      dirty: false,
      section_infos: initial_section_infos(notebook),
      cell_infos: initial_cell_infos(notebook),
      deleted_sections: [],
      deleted_cells: [],
      runtime: nil,
      client_pids: []
    }
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
        do: {cell.id, new_cell_info([])}
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

  def apply_operation(data, {:insert_section, _client_pid, index, id}) do
    section = %{Section.new() | id: id}

    data
    |> with_actions()
    |> insert_section(index, section)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:insert_cell, _client_pid, section_id, index, type, id}) do
    with {:ok, _section} <- Notebook.fetch_section(data.notebook, section_id) do
      cell = %{Cell.new(type) | id: id}

      data
      |> with_actions()
      |> insert_cell(section_id, index, cell)
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:delete_section, _client_pid, id}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, id) do
      data
      |> with_actions()
      |> delete_section(section)
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:delete_cell, _client_pid, id}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id) do
      case data.cell_infos[cell.id].evaluation_status do
        :evaluating ->
          data
          |> with_actions()
          |> clear_section_evaluation(section)
          |> add_action({:stop_evaluation, section})

        :queued ->
          data
          |> with_actions()
          |> unqueue_cell_evaluation(cell, section)
          |> unqueue_dependent_cells_evaluation(cell, section)
          |> mark_dependent_cells_as_stale(cell)

        _ ->
          data
          |> with_actions()
          |> mark_dependent_cells_as_stale(cell)
      end
      |> delete_cell(cell)
      |> add_action({:forget_evaluation, cell, section})
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:move_cell, _client_pid, id, offset}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         true <- offset != 0 do
      data
      |> with_actions()
      |> move_cell(cell, section, offset)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:queue_cell_evaluation, _client_pid, id}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         :elixir <- cell.type,
         :ready <- data.cell_infos[cell.id].evaluation_status do
      data
      |> with_actions()
      |> queue_prerequisite_cells_evaluation(cell, section)
      |> queue_cell_evaluation(cell, section)
      |> maybe_evaluate_queued()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:add_cell_evaluation_stdout, _client_pid, id, string}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, id),
         :evaluating <- data.cell_infos[cell.id].evaluation_status do
      data
      |> with_actions()
      |> add_cell_evaluation_stdout(cell, string)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:add_cell_evaluation_response, _client_pid, id, response}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         :evaluating <- data.cell_infos[cell.id].evaluation_status do
      data
      |> with_actions()
      |> add_cell_evaluation_response(cell, response)
      |> finish_cell_evaluation(cell, section)
      |> mark_dependent_cells_as_stale(cell)
      |> maybe_evaluate_queued()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:cancel_cell_evaluation, _client_pid, id}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id) do
      case data.cell_infos[cell.id].evaluation_status do
        :evaluating ->
          data
          |> with_actions()
          |> clear_section_evaluation(section)
          |> add_action({:stop_evaluation, section})
          |> wrap_ok()

        :queued ->
          data
          |> with_actions()
          |> unqueue_cell_evaluation(cell, section)
          |> unqueue_dependent_cells_evaluation(cell, section)
          |> mark_dependent_cells_as_stale(cell)
          |> wrap_ok()

        _ ->
          :error
      end
    end
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

  def apply_operation(data, {:client_join, pid}) do
    with false <- pid in data.client_pids do
      data
      |> with_actions()
      |> client_join(pid)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:client_leave, pid}) do
    with true <- pid in data.client_pids do
      data
      |> with_actions()
      |> client_leave(pid)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:apply_cell_delta, client_pid, cell_id, delta, revision}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         cell_info <- data.cell_infos[cell.id],
         true <- 0 < revision and revision <= cell_info.revision + 1,
         true <- client_pid in data.client_pids do
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
         true <- client_pid in data.client_pids do
      data
      |> with_actions()
      |> report_revision(client_pid, cell, revision)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:set_runtime, _client_pid, runtime}) do
    data
    |> with_actions()
    |> set!(runtime: runtime)
    |> reduce(data.notebook.sections, &clear_section_evaluation/2)
    |> wrap_ok()
  end

  def apply_operation(data, {:set_path, _client_pid, path}) do
    data
    |> with_actions()
    |> set!(path: path)
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

  defp insert_section({data, _} = data_actions, index, section) do
    data_actions
    |> set!(
      notebook: Notebook.insert_section(data.notebook, index, section),
      section_infos: Map.put(data.section_infos, section.id, new_section_info())
    )
  end

  defp insert_cell({data, _} = data_actions, section_id, index, cell) do
    data_actions
    |> set!(
      notebook: Notebook.insert_cell(data.notebook, section_id, index, cell),
      cell_infos: Map.put(data.cell_infos, cell.id, new_cell_info(data.client_pids))
    )
  end

  defp delete_section({data, _} = data_actions, section) do
    data_actions
    |> set!(
      notebook: Notebook.delete_section(data.notebook, section.id),
      section_infos: Map.delete(data.section_infos, section.id),
      deleted_sections: [section | data.deleted_sections]
    )
    |> reduce(section.cells, &delete_cell_info/2)
  end

  defp delete_cell({data, _} = data_actions, cell) do
    data_actions
    |> set!(
      notebook: Notebook.delete_cell(data.notebook, cell.id),
      deleted_cells: [cell | data.deleted_cells]
    )
    |> delete_cell_info(cell)
  end

  defp delete_cell_info({data, _} = data_actions, cell) do
    data_actions
    |> set!(cell_infos: Map.delete(data.cell_infos, cell.id))
  end

  defp move_cell({data, _} = data_actions, cell, section, offset) do
    idx = Enum.find_index(section.cells, &(&1 == cell))
    new_idx = (idx + offset) |> clamp_index(section.cells)

    updated_notebook = Notebook.move_cell(data.notebook, section.id, idx, new_idx)
    {:ok, updated_section} = Notebook.fetch_section(updated_notebook, section.id)

    elixir_cell_ids_before = elixir_cell_ids(section.cells)
    elixir_cell_ids_after = elixir_cell_ids(updated_section.cells)

    # If the order of Elixir cells stays the same, no need to invalidate anything
    affected_cells =
      if elixir_cell_ids_before != elixir_cell_ids_after do
        affected_from_idx = min(idx, new_idx)
        Enum.slice(updated_section.cells, affected_from_idx..-1)
      else
        []
      end

    data_actions
    |> set!(notebook: updated_notebook)
    |> mark_cells_as_stale(affected_cells)
    |> unqueue_cells_evaluation(affected_cells, section)
  end

  defp elixir_cell_ids(cells) do
    cells
    |> Enum.filter(&(&1.type == :elixir))
    |> Enum.map(& &1.id)
  end

  defp clamp_index(index, list) do
    index |> max(0) |> min(length(list) - 1)
  end

  defp queue_cell_evaluation(data_actions, cell, section) do
    data_actions
    |> update_section_info!(section.id, fn section ->
      %{section | evaluation_queue: section.evaluation_queue ++ [cell.id]}
    end)
    |> set_cell_info!(cell.id, evaluation_status: :queued)
  end

  defp unqueue_cell_evaluation(data_actions, cell, section) do
    data_actions
    |> update_section_info!(section.id, fn section ->
      %{section | evaluation_queue: List.delete(section.evaluation_queue, cell.id)}
    end)
    |> set_cell_info!(cell.id, evaluation_status: :ready)
  end

  defp add_cell_evaluation_stdout({data, _} = data_actions, cell, string) do
    data_actions
    |> set!(
      notebook:
        Notebook.update_cell(data.notebook, cell.id, fn cell ->
          %{cell | outputs: add_output(cell.outputs, string)}
        end)
    )
  end

  defp add_cell_evaluation_response({data, _} = data_actions, cell, response) do
    data_actions
    |> set!(
      notebook:
        Notebook.update_cell(data.notebook, cell.id, fn cell ->
          %{cell | outputs: add_output(cell.outputs, response)}
        end)
    )
  end

  defp add_output([], output), do: [output]

  defp add_output([head | tail], output) when is_binary(head) and is_binary(output) do
    # Merge consecutive string outputs
    [head <> output | tail]
  end

  defp add_output(outputs, output), do: [output | outputs]

  defp finish_cell_evaluation(data_actions, cell, section) do
    data_actions
    |> set_cell_info!(cell.id,
      validity_status: :evaluated,
      evaluation_status: :ready,
      evaluated_at: DateTime.utc_now()
    )
    |> set_section_info!(section.id, evaluating_cell_id: nil)
  end

  defp mark_dependent_cells_as_stale({data, _} = data_actions, cell) do
    child_cells = Notebook.child_cells(data.notebook, cell.id)
    mark_cells_as_stale(data_actions, child_cells)
  end

  defp mark_cells_as_stale({data, _} = data_actions, cells) do
    invalidated_cells =
      Enum.filter(cells, fn cell ->
        cell.type == :elixir and data.cell_infos[cell.id].validity_status == :evaluated
      end)

    data_actions
    |> reduce(invalidated_cells, &set_cell_info!(&1, &2.id, validity_status: :stale))
  end

  # If there are idle sections with non-empty evaluation queue,
  # the next queued cell for evaluation.
  defp maybe_evaluate_queued({data, _} = data_actions) do
    Enum.reduce(data.notebook.sections, data_actions, fn section, data_actions ->
      {data, _} = data_actions

      case data.section_infos[section.id] do
        %{evaluating_cell_id: nil, evaluation_queue: [id | ids]} ->
          cell = Enum.find(section.cells, &(&1.id == id))

          data_actions
          |> set!(notebook: Notebook.update_cell(data.notebook, id, &%{&1 | outputs: []}))
          |> set_cell_info!(id, evaluation_status: :evaluating)
          |> set_section_info!(section.id, evaluating_cell_id: id, evaluation_queue: ids)
          |> add_action({:start_evaluation, cell, section})

        _ ->
          data_actions
      end
    end)
  end

  defp clear_section_evaluation(data_actions, section) do
    data_actions
    |> set_section_info!(section.id, evaluating_cell_id: nil, evaluation_queue: [])
    |> reduce(
      section.cells,
      &set_cell_info!(&1, &2.id, validity_status: :fresh, evaluation_status: :ready)
    )
  end

  defp queue_prerequisite_cells_evaluation({data, _} = data_actions, cell, section) do
    prerequisites_queue =
      data.notebook
      |> Notebook.parent_cells(cell.id)
      |> Enum.take_while(fn parent ->
        info = data.cell_infos[parent.id]
        info.validity_status == :fresh and info.evaluation_status == :ready
      end)
      |> Enum.reverse()

    data_actions
    |> reduce(prerequisites_queue, &queue_cell_evaluation(&1, &2, section))
  end

  defp unqueue_dependent_cells_evaluation({data, _} = data_actions, cell, section) do
    dependent_cells = Notebook.child_cells(data.notebook, cell.id)
    unqueue_cells_evaluation(data_actions, dependent_cells, section)
  end

  defp unqueue_cells_evaluation({data, _} = data_actions, cells, section) do
    queued_cells =
      Enum.filter(cells, fn cell -> data.cell_infos[cell.id].evaluation_status == :queued end)

    data_actions
    |> reduce(queued_cells, &unqueue_cell_evaluation(&1, &2, section))
  end

  defp set_notebook_name({data, _} = data_actions, name) do
    data_actions
    |> set!(notebook: %{data.notebook | name: name})
  end

  defp set_section_name({data, _} = data_actions, section, name) do
    data_actions
    |> set!(notebook: Notebook.update_section(data.notebook, section.id, &%{&1 | name: name}))
  end

  defp client_join({data, _} = data_actions, pid) do
    data_actions
    |> set!(client_pids: [pid | data.client_pids])
    |> update_every_cell_info(fn info ->
      put_in(info.revision_by_client_pid[pid], info.revision)
    end)
  end

  defp client_leave({data, _} = data_actions, pid) do
    data_actions
    |> set!(client_pids: List.delete(data.client_pids, pid))
    |> update_every_cell_info(fn info ->
      {_, info} = pop_in(info.revision_by_client_pid[pid])
      purge_deltas(info)
    end)
  end

  defp apply_delta({data, _} = data_actions, client_pid, cell, delta, revision) do
    info = data.cell_infos[cell.id]

    deltas_ahead = Enum.take(info.deltas, -(info.revision - revision + 1))

    transformed_new_delta =
      Enum.reduce(deltas_ahead, delta, fn delta_ahead, transformed_new_delta ->
        Delta.transform(delta_ahead, transformed_new_delta, :left)
      end)

    new_source = JSInterop.apply_delta_to_string(transformed_new_delta, cell.source)

    data_actions
    |> set!(notebook: Notebook.update_cell(data.notebook, cell.id, &%{&1 | source: new_source}))
    |> update_cell_info!(cell.id, fn info ->
      info = %{info | deltas: info.deltas ++ [transformed_new_delta], revision: info.revision + 1}
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

  defp add_action({data, actions}, action) do
    {data, actions ++ [action]}
  end

  defp new_section_info() do
    %{
      evaluating_cell_id: nil,
      evaluation_queue: []
    }
  end

  defp new_cell_info(client_pids) do
    %{
      revision: 0,
      deltas: [],
      revision_by_client_pid: Map.new(client_pids, &{&1, 0}),
      validity_status: :fresh,
      evaluation_status: :ready,
      evaluated_at: nil
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

  @doc """
  Finds the cell that's currently being evaluated in the given section.
  """
  @spec get_evaluating_cell_id(t(), Section.id()) :: Cell.id() | nil
  def get_evaluating_cell_id(data, section_id) do
    info = data.section_infos[section_id]
    info && info.evaluating_cell_id
  end
end
