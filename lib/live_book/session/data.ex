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
    :status,
    :path,
    :execution_queue,
    :section_infos,
    :cell_infos,
    :deleted_sections,
    :deleted_cells
  ]

  alias LiveBook.{Notebook, Evaluator}
  alias LiveBook.Notebook.{Cell, Section}

  @type t :: %__MODULE__{
          notebook: Notebook.t(),
          status: status(),
          path: nil | String.t(),
          execution_queue: list(Cell.cell_id()),
          section_infos: %{Section.section_id() => section_info()},
          cell_infos: %{Cell.cell_id() => cell_info()},
          deleted_sections: list(Section.t()),
          deleted_cells: list(Cell.t())
        }

  @type status :: :idle | :evaluating

  @type section_info :: %{}

  @type cell_info :: %{
          status: cell_status(),
          revision: non_neg_integer(),
          # TODO: specify it's a list of deltas, once defined
          deltas: list(),
          evaluated_at: DateTime.t()
        }

  @type cell_status :: :fresh | :queued | :evaluating | :evaluated | :stale

  @type index :: non_neg_integer()

  @type operation ::
          {:insert_section, index(), Section.section_id()}
          | {:insert_cell, Section.section_id(), index(), Cell.cell_type(), Cell.cell_id()}
          | {:delete_section, Section.section_id()}
          | {:delete_cell, Cell.cell_id()}
          | {:queue_cell_evaluation, Cell.cell_id()}
          | {:add_cell_evaluation_stdout, Cell.cell_id(), String.t()}
          | {:add_cell_evaluation_response, Cell.cell_id(), Evaluator.evaluation_response()}

  @doc """
  Returns a fresh notebook session state.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      notebook: Notebook.new(),
      status: :idle,
      path: nil,
      execution_queue: [],
      section_infos: %{},
      cell_infos: %{},
      deleted_sections: [],
      deleted_cells: []
    }
  end

  @doc """
  Applies the change specified by `operation` to the given session `data`.

  All operations are reproducible (i.e. this function is pure),
  so provided all processes have the same session data
  they can individually apply any given operation and end up in the same state.

  An operation only applies changes to the structure, but it doesn't trigger
  any actual processing. It's the responsibility of the session process to ensure
  the system reflects the new structure. For instance, when the status
  changes from `:idle` to `:evaluating`, the session process should take
  care of evaluating the appropriate cell.

  Returns `{:ok, data}` on correct application or `:error` if the operation
  is not valid. The `:error` is generally expected given the collaborative
  nature of sessions. For example if there are simultaneous deletion
  and evaluation operations on the same cell, we may perform delete first,
  in which case the evaluation is no longer valid (there's no cell with the given id).
  By returning `:error` we simply notify the caller that no changes were applied,
  so any related actions can be ignored.
  """
  @spec apply_operation(t(), operation()) :: {:ok, t()} | :error
  def apply_operation(data, operation)

  def apply_operation(data, {:insert_section, index, id}) do
    section = %{Section.new() | id: id}

    {:ok,
     %{
       data
       | notebook: Notebook.insert_section(data.notebook, index, section),
         section_infos: Map.put(data.section_infos, section.id, new_section_info())
     }}
  end

  def apply_operation(data, {:insert_cell, section_id, index, type, id}) do
    with {:ok, _section} <- Notebook.fetch_section(data.notebook, id) do
      cell = %{Cell.new(type) | id: id}

      {:ok,
       %{
         data
         | notebook: Notebook.insert_cell(data.notebook, section_id, index, cell),
           cell_infos: Map.put(data.cell_infos, cell.id, new_cell_info())
       }}
    end
  end

  def apply_operation(data, {:delete_section, id}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, id) do
      {:ok,
       %{
         data
         | notebook: Notebook.delete_section(data.notebook, id),
           section_infos: Map.delete(data.section_infos, section.id),
           cell_infos: Enum.reduce(section.cells, data.cell_infos, &Map.delete(&2, &1.id)),
           evaluation_queue:
             Enum.reduce(section.cells, data.evaluation_queue, &List.delete(&2, &1.id)),
           deleted_sections: [section | data.deleted_sections]
       }}
    end
  end

  def apply_operation(data, {:delete_cell, id}) do
    with {:ok, cell} <- Notebook.fetch_cell(data.notebook, id),
         # If the cell is being evaluated, it should be cancelled first.
         false <- data.cell_infos[cell.id].status == :evaluating do
      {:ok,
       %{
         data
         | notebook: Notebook.delete_cell(data.notebook, id),
           evaluation_queue: List.delete(data.evaluation_queue, cell.id),
           cell_infos: Map.delete(data.cell_infos, cell.id),
           deleted_cells: [cell | data.deleted_cells]
       }}
    end
  end

  def apply_operation(data, {:queue_cell_evaluation, id}) do
    with {:ok, cell} <- Notebook.fetch_cell(data.notebook, id),
         false <- id in data.evaluation_queue do
      {:ok,
       %{
         data
         | evaluation_queue: data.evaluation_queue ++ [cell.id],
           cell_infos: Map.update!(data.cell_infos, cell.id, &%{&1 | status: :queued})
       }
       |> maybe_evaluate_queued()}
    end
  end

  def apply_operation(data, {:add_cell_evaluation_stdout, id, string}) do
    {:ok,
     %{
       data
       | # TODO: add stdout to cell outputs
         notebook: data.notebook
     }}
  end

  def apply_operation(data, {:add_cell_evaluation_response, id, response}) do
    with {:ok, cell} <- Notebook.fetch_cell(data.notebook, id) do
      child_cells = Notebook.child_cells(data.notebook, id)

      {:ok,
       %{
         data
         | status: :idle,
           # TODO: add response to cell outputs
           notebook: data.notebook,
           cell_infos:
             Enum.reduce(child_cells, data.cell_infos, fn cell, infos ->
               Map.update!(infos, cell.id, &%{&1 | status: :stale})
             end)
             |> Map.update!(cell.id, &%{&1 | status: :evaluated})
       }
       |> maybe_evaluate_queued()}
    end
  end

  defp new_section_info() do
    %{}
  end

  defp new_cell_info() do
    %{
      revision: 0,
      deltas: [],
      status: :idle,
      evaluated_at: nil
    }
  end

  # Given an idle session with non-empty evaluation queue,
  # changes the data to reflect evaluation of the first cell in the queue.
  defp maybe_evaluate_queued(%{status: :idle, evaluation_queue: [id | ids]} = data) do
    %{
      data
      | status: :evaluating,
        evaluation_queue: ids,
        notebook: Notebook.update_cell(data.notebook, id, &%{&1 | outputs: []}),
        cell_infos: Map.update!(data.cell_infos, id, &%{&1 | status: :evaluating})
    }
  end

  defp maybe_evaluate_queued(data), do: data

  @doc """
  Finds the cell that's currently being evaluated.
  """
  @spec get_evaluating_cell_id(t()) :: Cell.cell_id() | nil
  def get_evaluating_cell_id(data) do
    Enum.find(data.cell_infos, fn {_, info} -> info.status == :evaluating end)
    |> case do
      nil -> nil
      {cell_id, _} -> cell_id
    end
  end
end
