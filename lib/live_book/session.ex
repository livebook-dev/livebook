defmodule LiveBook.Session do
  @moduledoc false

  # Server corresponding to a single notebook session.
  #
  # The process keeps the current notebook state and serves
  # as a source of truth that multiple clients talk to.
  # Receives update requests from the clients and notifies
  # them of any changes applied to the notebook.
  #
  # The core concept is the `Data` structure
  # to which we can apply reproducible opreations.
  # See `Data` for more information.

  use GenServer, restart: :temporary

  alias LiveBook.Session.Data
  alias LiveBook.{Evaluator, EvaluatorSupervisor, Utils, Notebook}
  alias LiveBook.Notebook.{Cell, Section}

  @type state :: %{
          session_id: id(),
          data: Data.t(),
          evaluators: %{Section.t() => Evaluator.t()},
          client_pids: list(pid())
        }

  @typedoc """
  An id assigned to every running session process.
  """
  @type id :: Utils.id()

  ## API

  @doc """
  Starts the server process and registers it globally using the `:global` module,
  so that it's identifiable by the given id.
  """
  @spec start_link(id()) :: GenServer.on_start()
  def start_link(session_id) do
    GenServer.start_link(__MODULE__, [session_id: session_id], name: name(session_id))
  end

  defp name(session_id) do
    {:global, {:session, session_id}}
  end

  @doc """
  Registers a session client, so that it receives updates from the server.

  The client process is automatically unregistered when it terminates.
  """
  @spec register_client(id(), pid()) :: :ok
  def register_client(session_id, pid) do
    GenServer.cast(name(session_id), {:register_client, pid})
  end

  @doc """
  Asynchronously sends section insertion request to the server.
  """
  @spec insert_section(id(), non_neg_integer()) :: :ok
  def insert_section(session_id, index) do
    GenServer.cast(name(session_id), {:insert_section, index})
  end

  @doc """
  Asynchronously sends cell insertion request to the server.
  """
  @spec insert_cell(id(), Section.id(), non_neg_integer(), Cell.type()) ::
          :ok
  def insert_cell(session_id, section_id, index, type) do
    GenServer.cast(name(session_id), {:insert_cell, section_id, index, type})
  end

  @doc """
  Asynchronously sends section deletion request to the server.
  """
  @spec delete_section(id(), Section.id()) :: :ok
  def delete_section(session_id, section_id) do
    GenServer.cast(name(session_id), {:delete_section, section_id})
  end

  @doc """
  Asynchronously sends cell deletion request to the server.
  """
  @spec delete_cell(id(), Cell.id()) :: :ok
  def delete_cell(session_id, cell_id) do
    GenServer.cast(name(session_id), {:delete_cell, cell_id})
  end

  @doc """
  Asynchronously sends cell evaluation request to the server.
  """
  @spec queue_cell_evaluation(id(), Cell.id()) :: :ok
  def queue_cell_evaluation(session_id, cell_id) do
    GenServer.cast(name(session_id), {:queue_cell_evaluation, cell_id})
  end

  @doc """
  Synchronously stops the server.
  """
  @spec stop(id()) :: :ok
  def stop(session_id) do
    GenServer.stop(name(session_id))
  end

  ## Callbacks

  @impl true
  def init(session_id: session_id) do
    {:ok,
     %{
       session_id: session_id,
       data: Data.new(),
       evaluators: %{},
       client_pids: []
     }}
  end

  @impl true
  def handle_cast({:register_client, pid}, state) do
    Process.monitor(pid)
    {:noreply, %{state | client_pids: [pid | state.client_pids]}}
  end

  def handle_cast({:insert_section, index}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:insert_section, index, Utils.random_id()}
    handle_operation(state, operation)
  end

  def handle_cast({:insert_cell, section_id, index, type}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:insert_cell, section_id, index, type, Utils.random_id()}
    handle_operation(state, operation)
  end

  def handle_cast({:delete_section, section_id}, state) do
    operation = {:delete_section, section_id}

    handle_operation(state, operation, fn new_state ->
      delete_section_evaluator(new_state, section_id)
    end)
  end

  def handle_cast({:delete_cell, cell_id}, state) do
    operation = {:delete_cell, cell_id}
    handle_operation(state, operation)
  end

  def handle_cast({:queue_cell_evaluation, cell_id}, state) do
    operation = {:queue_cell_evaluation, cell_id}

    handle_operation(state, operation, fn new_state ->
      maybe_trigger_evaluations(state, new_state)
    end)
  end

  @impl true
  def handle_info({:DOWN, _, :process, pid, _}, state) do
    {:noreply, %{state | client_pids: List.delete(state.client_pids, pid)}}
  end

  def handle_info({:evaluator_stdout, cell_id, string}, state) do
    operation = {:add_cell_evaluation_stdout, cell_id, string}
    handle_operation(state, operation)
  end

  def handle_info({:evaluator_response, cell_id, response}, state) do
    operation = {:add_cell_evaluation_response, cell_id, response}

    handle_operation(state, operation, fn new_state ->
      maybe_trigger_evaluations(state, new_state)
    end)
  end

  # ---

  # Given any opeation on `Data`, the process does the following:
  #
  #   * broadcasts the operation to all clients immediately,
  #     so that they can update their local `Data`
  #   * applies the operation to own local `Data`
  #   * optionally performs a relevant task (e.g. starts cell evaluation),
  #     to reflect the new `Data`
  #
  defp handle_operation(state, operation) do
    handle_operation(state, operation, fn state -> state end)
  end

  defp handle_operation(state, operation, handle_new_state) do
    broadcast_operation(state.session_id, operation)

    case Data.apply_operation(state.data, operation) do
      {:ok, new_data} ->
        new_state = %{state | data: new_data}
        {:noreply, handle_new_state.(new_state)}

      :error ->
        {:noreply, state}
    end
  end

  defp broadcast_operation(session_id, operation) do
    message = {:operation, operation}
    Phoenix.PubSub.broadcast(LiveBook.PubSub, "sessions:#{session_id}", message)
  end

  # Compares sections in the old and new state and if a new cell
  # has been marked as evaluating it triggers the actual evaluation task.
  defp maybe_trigger_evaluations(old_state, new_state) do
    Enum.reduce(new_state.data.notebook.sections, new_state, fn section, state ->
      case {Data.get_evaluating_cell_id(old_state.data, section.id),
            Data.get_evaluating_cell_id(new_state.data, section.id)} do
        {_, nil} ->
          # No cell to evaluate
          state

        {cell_id, cell_id} ->
          # The evaluating cell hasn't changed, so it must be already evaluating
          state

        {_, cell_id} ->
          # The evaluating cell changed, so we trigger the evaluation to reflect that
          trigger_evaluation(state, cell_id)
      end
    end)
  end

  defp trigger_evaluation(state, cell_id) do
    notebook = state.data.notebook
    {:ok, cell, section} = Notebook.fetch_cell_and_section(notebook, cell_id)
    {state, evaluator} = get_section_evaluator(state, section.id)
    %{source: source} = cell

    prev_ref =
      case Notebook.parent_cells(notebook, cell_id) do
        [parent | _] -> parent.id
        [] -> :initial
      end

    Evaluator.evaluate_code(evaluator, self(), source, cell_id, prev_ref)

    state
  end

  defp get_section_evaluator(state, section_id) do
    case Map.fetch(state.evaluators, section_id) do
      {:ok, evaluator} ->
        {state, evaluator}

      :error ->
        {:ok, evaluator} = EvaluatorSupervisor.start_evaluator()
        state = %{state | evaluators: Map.put(state.evaluators, section_id, evaluator)}
        {state, evaluator}
    end
  end

  defp delete_section_evaluator(state, section_id) do
    case Map.fetch(state.evaluators, section_id) do
      {:ok, evaluator} ->
        EvaluatorSupervisor.terminate_evaluator(evaluator)
        %{state | evaluators: Map.delete(state.evaluators, section_id)}

      :error ->
        state
    end
  end
end
