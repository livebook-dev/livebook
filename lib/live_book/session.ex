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

  alias LiveBook.Session.{Data, FileGuard}
  alias LiveBook.{Utils, Notebook, Delta, Runtime, LiveMarkdown}
  alias LiveBook.Notebook.{Cell, Section}

  @type state :: %{
          session_id: id(),
          data: Data.t(),
          runtime_monitor_ref: reference()
        }

  @type summary :: %{
          session_id: id(),
          notebook_name: String.t(),
          path: String.t() | nil
        }

  @typedoc """
  An id assigned to every running session process.
  """
  @type id :: Utils.id()

  @autosave_interval 5_000

  ## API

  @doc """
  Starts the server process and registers it globally using the `:global` module,
  so that it's identifiable by the given id.

  ## Options

  * `:id` (**required**) - a unique identifier to register the session under

  * `:notebook` - the inital `Notebook` structure (e.g. imported from a file)

  * `:path` - the file to which the notebook should be saved
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    id = Keyword.fetch!(opts, :id)
    GenServer.start_link(__MODULE__, opts, name: name(id))
  end

  defp name(session_id) do
    {:global, {:session, session_id}}
  end

  @doc """
  Returns session pid given its id.
  """
  @spec get_pid(id()) :: pid() | nil
  def get_pid(session_id) do
    GenServer.whereis(name(session_id))
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
  Returns data of the given session.
  """
  @spec get_data(id()) :: Data.t()
  def get_data(session_id) do
    GenServer.call(name(session_id), :get_data)
  end

  @doc """
  Returns basic information about the given session.
  """
  @spec get_summary(id()) :: summary()
  def get_summary(session_id) do
    GenServer.call(name(session_id), :get_summary)
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
  Asynchronously sends a cell delta to apply to the server.
  """
  @spec apply_cell_delta(id(), pid(), Cell.id(), Delta.t(), Data.cell_revision()) :: :ok
  def apply_cell_delta(session_id, client_pid, cell_id, delta, revision) do
    GenServer.cast(name(session_id), {:apply_cell_delta, client_pid, cell_id, delta, revision})
  end

  @doc """
  Asynchronously informs at what revision the given client is.

  This helps to remove old deltas that are no longer necessary.
  """
  @spec report_cell_revision(id(), pid(), Cell.id(), Data.cell_revision()) :: :ok
  def report_cell_revision(session_id, client_pid, cell_id, revision) do
    GenServer.cast(name(session_id), {:report_cell_revision, client_pid, cell_id, revision})
  end

  @doc """
  Asynchronously connects to the given runtime.

  Note that this results in initializing the corresponding remote node
  with modules and processes required for evaluation.
  """
  @spec connect_runtime(id(), Runtime.t()) :: :ok
  def connect_runtime(session_id, runtime) do
    GenServer.cast(name(session_id), {:connect_runtime, runtime})
  end

  @doc """
  Asynchronously disconnects from the current runtime.

  Note that this results in clearing the evaluation state.
  """
  @spec disconnect_runtime(id()) :: :ok
  def disconnect_runtime(session_id) do
    GenServer.cast(name(session_id), :disconnect_runtime)
  end

  @doc """
  Asynchronously sends path update request to the server.
  """
  @spec set_path(id(), String.t() | nil) :: :ok
  def set_path(session_id, path) do
    GenServer.cast(name(session_id), {:set_path, path})
  end

  @doc """
  Asynchronously sends save request to the server.

  If there's a path set and the notebook changed since the last save,
  it will be persisted to said path.
  Note that notebooks are automatically persisted every @autosave_interval milliseconds.
  """
  @spec save(id()) :: :ok
  def save(session_id) do
    GenServer.cast(name(session_id), :save)
  end

  @doc """
  Asynchronously sends a close request to the server.

  This results in saving the file and broadcasting
  a :closed message to the session topic.
  """
  @spec close(id()) :: :ok
  def close(session_id) do
    GenServer.cast(name(session_id), :close)
  end

  ## Callbacks

  @impl true
  def init(opts) do
    Process.send_after(self(), :autosave, @autosave_interval)

    id = Keyword.fetch!(opts, :id)

    case init_data(opts) do
      {:ok, data} ->
        {:ok,
         %{
           session_id: id,
           data: data,
           runtime_monitor_ref: nil
         }}

      {:error, error} ->
        {:stop, error}
    end
  end

  defp init_data(opts) do
    notebook = Keyword.get(opts, :notebook)
    path = Keyword.get(opts, :path)

    data = if(notebook, do: Data.new(notebook), else: Data.new())

    if path do
      case FileGuard.lock(path, self()) do
        :ok ->
          {:ok, %{data | path: path}}

        {:error, :already_in_use} ->
          {:error, "the given path is already in use"}
      end
    else
      {:ok, data}
    end
  end

  @impl true
  def handle_call({:register_client, pid}, _from, state) do
    Process.monitor(pid)

    state = handle_operation(state, {:client_join, pid})

    {:reply, state.data, state}
  end

  def handle_call(:get_data, _from, state) do
    {:reply, state.data, state}
  end

  def handle_call(:get_summary, _from, state) do
    {:reply, summary_from_state(state), state}
  end

  @impl true
  def handle_cast({:insert_section, index}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:insert_section, index, Utils.random_id()}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:insert_cell, section_id, index, type}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:insert_cell, section_id, index, type, Utils.random_id()}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:delete_section, section_id}, state) do
    operation = {:delete_section, section_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:delete_cell, cell_id}, state) do
    operation = {:delete_cell, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:queue_cell_evaluation, cell_id}, state) do
    case ensure_runtime(state) do
      {:ok, state} ->
        operation = {:queue_cell_evaluation, cell_id}
        {:noreply, handle_operation(state, operation)}

      {:error, error} ->
        broadcast_error(state.session_id, "failed to setup runtime - #{error}")
        {:noreply, state}
    end
  end

  def handle_cast({:cancel_cell_evaluation, cell_id}, state) do
    operation = {:cancel_cell_evaluation, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_notebook_name, name}, state) do
    operation = {:set_notebook_name, name}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_section_name, section_id, name}, state) do
    operation = {:set_section_name, section_id, name}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:apply_cell_delta, client_pid, cell_id, delta, revision}, state) do
    operation = {:apply_cell_delta, client_pid, cell_id, delta, revision}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:report_cell_revision, client_pid, cell_id, revision}, state) do
    operation = {:report_cell_revision, client_pid, cell_id, revision}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:connect_runtime, runtime}, state) do
    if state.data.runtime do
      Runtime.disconnect(state.data.runtime)
    end

    runtime_monitor_ref = Runtime.connect(runtime)

    {:noreply,
     %{state | runtime_monitor_ref: runtime_monitor_ref}
     |> handle_operation({:set_runtime, runtime})}
  end

  def handle_cast(:disconnect_runtime, state) do
    Runtime.disconnect(state.data.runtime)

    {:noreply,
     %{state | runtime_monitor_ref: nil}
     |> handle_operation({:set_runtime, nil})}
  end

  def handle_cast({:set_path, path}, state) do
    if path do
      FileGuard.lock(path, self())
    else
      :ok
    end
    |> case do
      :ok ->
        if state.data.path do
          FileGuard.unlock(state.data.path)
        end

        {:noreply, handle_operation(state, {:set_path, path})}

      {:error, :already_in_use} ->
        broadcast_error(state.session_id, "failed to set new path because it is already in use")
        {:noreply, state}
    end
  end

  def handle_cast(:save, state) do
    {:noreply, maybe_save_notebook(state)}
  end

  def handle_cast(:close, state) do
    maybe_save_notebook(state)
    broadcast_message(state.session_id, :session_closed)

    {:stop, :shutdown, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, _}, %{runtime_monitor_ref: ref} = state) do
    broadcast_info(state.session_id, "runtime node terminated unexpectedly")

    {:noreply,
     %{state | runtime_monitor_ref: nil}
     |> handle_operation({:set_runtime, nil})}
  end

  def handle_info({:DOWN, _, :process, pid, _}, state) do
    state =
      if pid in state.data.client_pids do
        handle_operation(state, {:client_leave, pid})
      else
        state
      end

    {:noreply, state}
  end

  def handle_info({:evaluation_stdout, cell_id, string}, state) do
    operation = {:add_cell_evaluation_stdout, cell_id, string}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info({:evaluation_response, cell_id, response}, state) do
    operation = {:add_cell_evaluation_response, cell_id, response}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info(:autosave, state) do
    Process.send_after(self(), :autosave, @autosave_interval)
    {:noreply, maybe_save_notebook(state)}
  end

  def handle_info(_message, state), do: {:noreply, state}

  # ---

  defp summary_from_state(state) do
    %{
      session_id: state.session_id,
      notebook_name: state.data.notebook.name,
      path: state.data.path
    }
  end

  # Given any opeation on `Data`, the process does the following:
  #
  #   * broadcasts the operation to all clients immediately,
  #     so that they can update their local `Data`
  #   * applies the operation to own local `Data`
  #   * if necessary, performs the relevant actions (e.g. starts cell evaluation),
  #     to reflect the new `Data`
  #
  defp handle_operation(state, operation) do
    broadcast_operation(state.session_id, operation)

    case Data.apply_operation(state.data, operation) do
      {:ok, new_data, actions} ->
        new_state = %{state | data: new_data}
        handle_actions(new_state, actions)

      :error ->
        state
    end
  end

  defp handle_actions(state, actions) do
    Enum.reduce(actions, state, &handle_action(&2, &1))
  end

  defp handle_action(state, {:start_evaluation, cell, section}) do
    start_evaluation(state, cell, section)
  end

  defp handle_action(state, {:stop_evaluation, section}) do
    if state.data.runtime do
      Runtime.drop_container(state.data.runtime, section.id)
    end

    state
  end

  defp handle_action(state, {:forget_evaluation, cell, section}) do
    if state.data.runtime do
      Runtime.forget_evaluation(state.data.runtime, section.id, cell.id)
    end

    state
  end

  defp handle_action(state, _action), do: state

  defp broadcast_operation(session_id, operation) do
    broadcast_message(session_id, {:operation, operation})
  end

  defp broadcast_error(session_id, error) do
    broadcast_message(session_id, {:error, error})
  end

  defp broadcast_info(session_id, info) do
    broadcast_message(session_id, {:info, info})
  end

  defp broadcast_message(session_id, message) do
    Phoenix.PubSub.broadcast(LiveBook.PubSub, "sessions:#{session_id}", message)
  end

  defp start_evaluation(state, cell, section) do
    prev_ref =
      case Notebook.parent_cells(state.data.notebook, cell.id) do
        [parent | _] -> parent.id
        [] -> :initial
      end

    Runtime.evaluate_code(state.data.runtime, cell.source, section.id, cell.id, prev_ref)

    state
  end

  # Checks if a runtime already set, and if that's not the case
  # starts a new standlone one.
  defp ensure_runtime(%{data: %{runtime: nil}} = state) do
    with {:ok, runtime} <- Runtime.Standalone.init(self()) do
      runtime_monitor_ref = Runtime.connect(runtime)

      {:ok,
       %{state | runtime_monitor_ref: runtime_monitor_ref}
       |> handle_operation({:set_runtime, runtime})}
    end
  end

  defp ensure_runtime(state), do: {:ok, state}

  defp maybe_save_notebook(state) do
    if state.data.path != nil and state.data.dirty do
      content = LiveMarkdown.Export.notebook_to_markdown(state.data.notebook)

      case File.write(state.data.path, content) do
        :ok ->
          handle_operation(state, :mark_as_not_dirty)

        {:error, reason} ->
          broadcast_error(state.session_id, "failed to save notebook - #{reason}")
          state
      end
    else
      state
    end
  end
end
