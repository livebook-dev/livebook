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
  Registers a session client, so that the session is aware of it.

  The client process is automatically unregistered when it terminates.

  Returns the current session data, which the client can than
  keep in sync with the server by subscribing to the `sessions:id` topic
  and reciving operations to apply.
  """
  @spec register_client(id(), pid()) :: Data.t()
  def register_client(session_id, pid) do
    GenServer.call(name(session_id), {:register_client, pid})
  end

  @doc """
  Returns the current session data.
  """
  @spec get_data(id()) :: Data.t()
  def get_data(session_id) do
    GenServer.call(name(session_id), :get_data)
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
  Asynchronously sends cell evaluation cancellation request to the server.
  """
  @spec cancel_cell_evaluation(id(), Cell.id()) :: :ok
  def cancel_cell_evaluation(session_id, cell_id) do
    GenServer.cast(name(session_id), {:cancel_cell_evaluation, cell_id})
  end

  @doc """
  Asynchronously sends notebook name update request to the server.
  """
  @spec set_notebook_name(id(), String.t()) :: :ok
  def set_notebook_name(session_id, name) do
    GenServer.cast(name(session_id), {:set_notebook_name, name})
  end

  @doc """
  Asynchronously sends section name update request to the server.
  """
  @spec set_section_name(id(), Section.id(), String.t()) :: :ok
  def set_section_name(session_id, section_id, name) do
    GenServer.cast(name(session_id), {:set_section_name, section_id, name})
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
  def handle_call({:register_client, pid}, _from, state) do
    Process.monitor(pid)
    {:reply, state.data, %{state | client_pids: [pid | state.client_pids]}}
  end

  def handle_call(:get_data, _from, state) do
    {:reply, state.data, state}
  end

  @impl true
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
    handle_operation(state, operation)
  end

  def handle_cast({:delete_cell, cell_id}, state) do
    operation = {:delete_cell, cell_id}
    handle_operation(state, operation)
  end

  def handle_cast({:queue_cell_evaluation, cell_id}, state) do
    operation = {:queue_cell_evaluation, cell_id}
    handle_operation(state, operation)
  end

  def handle_cast({:cancel_cell_evaluation, cell_id}, state) do
    operation = {:cancel_cell_evaluation, cell_id}
    handle_operation(state, operation)
  end

  def handle_cast({:set_notebook_name, name}, state) do
    operation = {:set_notebook_name, name}
    handle_operation(state, operation)
  end

  def handle_cast({:set_section_name, section_id, name}, state) do
    operation = {:set_section_name, section_id, name}
    handle_operation(state, operation)
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
    handle_operation(state, operation)
  end

  # ---

  # Given any opeation on `Data`, the process does the following:
  #
  #   * broadcasts the operation to all clients immediately,
  #     so that they can update their local `Data`
  #   * applies the operation to own local `Data`
  #   * if necessary, performs the relevant tasks (e.g. starts cell evaluation),
  #     to reflect the new `Data`
  #
  defp handle_operation(state, operation) do
    broadcast_operation(state.session_id, operation)

    case Data.apply_operation(state.data, operation) do
      {:ok, new_data, actions} ->
        new_state = %{state | data: new_data}
        {:noreply, handle_actions(new_state, actions)}

      :error ->
        {:noreply, state}
    end
  end

  defp handle_actions(state, actions) do
    Enum.reduce(actions, state, &handle_action(&2, &1))
  end

  defp handle_action(state, {:start_evaluation, cell, section}) do
    trigger_evaluation(state, cell, section)
  end

  defp handle_action(state, {:stop_evaluation, section}) do
    delete_section_evaluator(state, section.id)
  end

  defp handle_action(state, {:forget_evaluation, cell, section}) do
    with {:ok, evaluator} <- fetch_section_evaluator(state, section.id) do
      Evaluator.forget_evaluation(evaluator, cell.id)
    end

    state
  end

  defp broadcast_operation(session_id, operation) do
    message = {:operation, operation}
    Phoenix.PubSub.broadcast(LiveBook.PubSub, "sessions:#{session_id}", message)
  end

  defp trigger_evaluation(state, cell, section) do
    {state, evaluator} = get_section_evaluator(state, section.id)

    prev_ref =
      case Notebook.parent_cells(state.data.notebook, cell.id) do
        [parent | _] -> parent.id
        [] -> :initial
      end

    Evaluator.evaluate_code(evaluator, self(), cell.source, cell.id, prev_ref)

    state
  end

  defp fetch_section_evaluator(state, section_id) do
    Map.fetch(state.evaluators, section_id)
  end

  defp get_section_evaluator(state, section_id) do
    case fetch_section_evaluator(state, section_id) do
      {:ok, evaluator} ->
        {state, evaluator}

      :error ->
        {:ok, evaluator} = EvaluatorSupervisor.start_evaluator()
        state = %{state | evaluators: Map.put(state.evaluators, section_id, evaluator)}
        {state, evaluator}
    end
  end

  defp delete_section_evaluator(state, section_id) do
    case fetch_section_evaluator(state, section_id) do
      {:ok, evaluator} ->
        EvaluatorSupervisor.terminate_evaluator(evaluator)
        %{state | evaluators: Map.delete(state.evaluators, section_id)}

      :error ->
        state
    end
  end
end
