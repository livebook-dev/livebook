defmodule Livebook.Session do
  @moduledoc false

  # Server corresponding to a single notebook session.
  #
  # The process keeps the current notebook state and serves
  # as a source of truth that multiple clients talk to.
  # Receives update requests from the clients and notifies
  # them of any changes applied to the notebook.
  #
  # ## Collaborative state
  #
  # The core concept is the `Livebook.Session.Data` structure
  # to which we can apply reproducible operations.
  # See `Livebook.Session.Data` for more information.
  #
  # ## Evaluation
  #
  # All regular sections are evaluated in the same process
  # (the :main_flow evaluation container). On the other hand,
  # each branching section is evaluated in its own process
  # and thus runs concurrently.
  #
  # ### Implementation considerations
  #
  # In practice, every evaluation container is a `Livebook.Evaluator`
  # process, so we have one such process for the main flow and one
  # for each branching section. Since a branching section inherits
  # the evaluation context from the parent section, the last context
  # needs to be copied from the main flow evaluator to the branching
  # section evaluator. The latter synchronously asks the former for
  # that context using `Livebook.Evaluator.fetch_evaluation_context/3`.
  # Consequently, in order to evaluate the first cell in a branching
  # section, the main flow needs to be free of work, otherwise we wait.
  # This assumptions are mirrored in by `Livebook.Session.Data` when
  # determining cells for evaluation.
  #
  # Note: the context could be copied asynchronously if evaluator
  # kept the contexts in its process dictionary, however the other
  # evaluator could only read the whole process dictionary, thus
  # allocating a lot of memory unnecessarily, which would be unacceptable
  # for large data. By making a synchronous request to the evalutor
  # for a single specific evaluation context we make sure to copy
  # as little memory as necessary.

  # The struct holds the basic session information that we track
  # and pass around. The notebook and evaluation state is kept
  # within the process state.
  defstruct [:id, :pid, :origin, :notebook_name, :file, :images_dir, :created_at]

  use GenServer, restart: :temporary

  alias Livebook.Session.{Data, FileGuard}
  alias Livebook.{Utils, Notebook, Delta, Runtime, LiveMarkdown, FileSystem}
  alias Livebook.Users.User
  alias Livebook.Notebook.{Cell, Section}

  @type t :: %__MODULE__{
          id: id(),
          pid: pid(),
          origin: {:file, FileSystem.File.t()} | {:url, String.t()} | nil,
          notebook_name: String.t(),
          file: FileSystem.File.t() | nil,
          images_dir: FileSystem.File.t(),
          created_at: DateTime.t()
        }

  @type state :: %{
          session_id: id(),
          data: Data.t(),
          created_at: DateTime.t(),
          runtime_monitor_ref: reference() | nil,
          autosave_timer_ref: reference() | nil,
          save_task_pid: pid() | nil
        }

  @typedoc """
  An id assigned to every running session process.
  """
  @type id :: Utils.id()

  ## API

  @doc """
  Starts a session server process.

  ## Options

    * `:id` (**required**) - a unique session identifier

    * `:notebook` - the initial `Notebook` structure (e.g. imported from a file)

    * `:origin` - location from where the notebook was obtained, can be either
      `{:file, file}`, a remote `{:url, url}`, or `nil`

    * `:file` - the file to which the notebook should be saved

    * `:copy_images_from` - a directory file to copy notebook images from

    * `:images` - a map from image name to its binary content, an alternative
      to `:copy_images_from` when the images are in memory
  """
  @spec start_link(keyword()) :: {:ok, pid} | {:error, any()}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Fetches session information from the session server.
  """
  @spec get_by_pid(pid()) :: Session.t()
  def get_by_pid(pid) do
    GenServer.call(pid, :describe_self)
  end

  @doc """
  Registers a session client, so that the session is aware of it.

  The client process is automatically unregistered when it terminates.

  Returns the current session data, which the client can than
  keep in sync with the server by subscribing to the `sessions:id` topic
  and receiving operations to apply.
  """
  @spec register_client(pid(), pid(), User.t()) :: Data.t()
  def register_client(pid, client_pid, user) do
    GenServer.call(pid, {:register_client, client_pid, user})
  end

  @doc """
  Returns data of the given session.
  """
  @spec get_data(pid()) :: Data.t()
  def get_data(pid) do
    GenServer.call(pid, :get_data)
  end

  @doc """
  Returns the current notebook structure.
  """
  @spec get_notebook(pid()) :: Notebook.t()
  def get_notebook(pid) do
    GenServer.call(pid, :get_notebook)
  end

  @doc """
  Asynchronously sends notebook attributes update to the server.
  """
  @spec set_notebook_attributes(pid(), map()) :: :ok
  def set_notebook_attributes(pid, attrs) do
    GenServer.cast(pid, {:set_notebook_attributes, self(), attrs})
  end

  @doc """
  Asynchronously sends section insertion request to the server.
  """
  @spec insert_section(pid(), non_neg_integer()) :: :ok
  def insert_section(pid, index) do
    GenServer.cast(pid, {:insert_section, self(), index})
  end

  @doc """
  Asynchronously sends section insertion request to the server.
  """
  @spec insert_section_into(pid(), Section.id(), non_neg_integer()) :: :ok
  def insert_section_into(pid, section_id, index) do
    GenServer.cast(pid, {:insert_section_into, self(), section_id, index})
  end

  @doc """
  Asynchronously sends parent update request to the server.
  """
  @spec set_section_parent(pid(), Section.id(), Section.id()) :: :ok
  def set_section_parent(pid, section_id, parent_id) do
    GenServer.cast(pid, {:set_section_parent, self(), section_id, parent_id})
  end

  @doc """
  Asynchronously sends parent update request to the server.
  """
  @spec unset_section_parent(pid(), Section.id()) :: :ok
  def unset_section_parent(pid, section_id) do
    GenServer.cast(pid, {:unset_section_parent, self(), section_id})
  end

  @doc """
  Asynchronously sends cell insertion request to the server.
  """
  @spec insert_cell(pid(), Section.id(), non_neg_integer(), Cell.type()) :: :ok
  def insert_cell(pid, section_id, index, type) do
    GenServer.cast(pid, {:insert_cell, self(), section_id, index, type})
  end

  @doc """
  Asynchronously sends section deletion request to the server.
  """
  @spec delete_section(pid(), Section.id(), boolean()) :: :ok
  def delete_section(pid, section_id, delete_cells) do
    GenServer.cast(pid, {:delete_section, self(), section_id, delete_cells})
  end

  @doc """
  Asynchronously sends cell deletion request to the server.
  """
  @spec delete_cell(pid(), Cell.id()) :: :ok
  def delete_cell(pid, cell_id) do
    GenServer.cast(pid, {:delete_cell, self(), cell_id})
  end

  @doc """
  Asynchronously sends cell restoration request to the server.
  """
  @spec restore_cell(pid(), Cell.id()) :: :ok
  def restore_cell(pid, cell_id) do
    GenServer.cast(pid, {:restore_cell, self(), cell_id})
  end

  @doc """
  Asynchronously sends cell move request to the server.
  """
  @spec move_cell(pid(), Cell.id(), integer()) :: :ok
  def move_cell(pid, cell_id, offset) do
    GenServer.cast(pid, {:move_cell, self(), cell_id, offset})
  end

  @doc """
  Asynchronously sends section move request to the server.
  """
  @spec move_section(pid(), Section.id(), integer()) :: :ok
  def move_section(pid, section_id, offset) do
    GenServer.cast(pid, {:move_section, self(), section_id, offset})
  end

  @doc """
  Asynchronously sends cell evaluation request to the server.
  """
  @spec queue_cell_evaluation(pid(), Cell.id()) :: :ok
  def queue_cell_evaluation(pid, cell_id) do
    GenServer.cast(pid, {:queue_cell_evaluation, self(), cell_id})
  end

  @doc """
  Asynchronously sends cell evaluation cancellation request to the server.
  """
  @spec cancel_cell_evaluation(pid(), Cell.id()) :: :ok
  def cancel_cell_evaluation(pid, cell_id) do
    GenServer.cast(pid, {:cancel_cell_evaluation, self(), cell_id})
  end

  @doc """
  Asynchronously sends notebook name update request to the server.
  """
  @spec set_notebook_name(pid(), String.t()) :: :ok
  def set_notebook_name(pid, name) do
    GenServer.cast(pid, {:set_notebook_name, self(), name})
  end

  @doc """
  Asynchronously sends section name update request to the server.
  """
  @spec set_section_name(pid(), Section.id(), String.t()) :: :ok
  def set_section_name(pid, section_id, name) do
    GenServer.cast(pid, {:set_section_name, self(), section_id, name})
  end

  @doc """
  Asynchronously sends a cell delta to apply to the server.
  """
  @spec apply_cell_delta(pid(), Cell.id(), Delta.t(), Data.cell_revision()) :: :ok
  def apply_cell_delta(pid, cell_id, delta, revision) do
    GenServer.cast(pid, {:apply_cell_delta, self(), cell_id, delta, revision})
  end

  @doc """
  Asynchronously informs at what revision the given client is.

  This helps to remove old deltas that are no longer necessary.
  """
  @spec report_cell_revision(pid(), Cell.id(), Data.cell_revision()) :: :ok
  def report_cell_revision(pid, cell_id, revision) do
    GenServer.cast(pid, {:report_cell_revision, self(), cell_id, revision})
  end

  @doc """
  Asynchronously sends a cell attributes update to the server.
  """
  @spec set_cell_attributes(pid(), Cell.id(), map()) :: :ok
  def set_cell_attributes(pid, cell_id, attrs) do
    GenServer.cast(pid, {:set_cell_attributes, self(), cell_id, attrs})
  end

  @doc """
  Asynchronously connects to the given runtime.

  Note that this results in initializing the corresponding remote node
  with modules and processes required for evaluation.
  """
  @spec connect_runtime(pid(), Runtime.t()) :: :ok
  def connect_runtime(pid, runtime) do
    GenServer.cast(pid, {:connect_runtime, self(), runtime})
  end

  @doc """
  Asynchronously disconnects from the current runtime.

  Note that this results in clearing the evaluation state.
  """
  @spec disconnect_runtime(pid()) :: :ok
  def disconnect_runtime(pid) do
    GenServer.cast(pid, {:disconnect_runtime, self()})
  end

  @doc """
  Asynchronously sends file location update request to the server.
  """
  @spec set_file(pid(), FileSystem.File.t() | nil) :: :ok
  def set_file(pid, file) do
    GenServer.cast(pid, {:set_file, self(), file})
  end

  @doc """
  Asynchronously sends save request to the server.

  If there's a file set and the notebook changed since the last save,
  it will be persisted to said file.

  Note that notebooks are automatically persisted every @autosave_interval milliseconds.
  """
  @spec save(pid()) :: :ok
  def save(pid) do
    GenServer.cast(pid, :save)
  end

  @doc """
  Synchronous version of `save/1`.
  """
  @spec save_sync(pid()) :: :ok
  def save_sync(pid) do
    GenServer.call(pid, :save_sync)
  end

  @doc """
  Asynchronously sends a close request to the server.

  This results in saving the file and broadcasting
  a :closed message to the session topic.
  """
  @spec close(pid()) :: :ok
  def close(pid) do
    GenServer.cast(pid, :close)
  end

  ## Callbacks

  @impl true
  def init(opts) do
    with {:ok, state} <- init_state(opts),
         :ok <-
           if(copy_images_from = opts[:copy_images_from],
             do: copy_images(state, copy_images_from),
             else: :ok
           ),
         :ok <-
           if(images = opts[:images],
             do: dump_images(state, images),
             else: :ok
           ) do
      state = schedule_autosave(state)
      {:ok, state}
    else
      {:error, error} ->
        {:stop, error}
    end
  end

  defp init_state(opts) do
    id = Keyword.fetch!(opts, :id)

    with {:ok, data} <- init_data(opts) do
      state = %{
        session_id: id,
        data: data,
        created_at: DateTime.utc_now(),
        runtime_monitor_ref: nil,
        autosave_timer_ref: nil,
        save_task_pid: nil
      }

      {:ok, state}
    end
  end

  defp init_data(opts) do
    notebook = Keyword.get_lazy(opts, :notebook, &default_notebook/0)
    file = opts[:file]
    origin = opts[:origin]

    data = Data.new(notebook)
    data = %{data | origin: origin}

    if file do
      case FileGuard.lock(file, self()) do
        :ok ->
          {:ok, %{data | file: file}}

        {:error, :already_in_use} ->
          {:error, "the given file is already in use"}
      end
    else
      {:ok, data}
    end
  end

  defp default_notebook() do
    %{Notebook.new() | sections: [%{Section.new() | cells: [Cell.new(:elixir)]}]}
  end

  defp schedule_autosave(state) do
    if interval_s = state.data.notebook.autosave_interval_s do
      ref = Process.send_after(self(), :autosave, interval_s * 1000)
      %{state | autosave_timer_ref: ref}
    else
      %{state | autosave_timer_ref: nil}
    end
  end

  @impl true
  def handle_call(:describe_self, _from, state) do
    {:reply, self_from_state(state), state}
  end

  def handle_call({:register_client, client_pid, user}, _from, state) do
    Process.monitor(client_pid)

    state = handle_operation(state, {:client_join, client_pid, user})

    {:reply, state.data, state}
  end

  def handle_call(:get_data, _from, state) do
    {:reply, state.data, state}
  end

  def handle_call(:get_notebook, _from, state) do
    {:reply, state.data.notebook, state}
  end

  def handle_call(:save_sync, _from, state) do
    {:reply, :ok, maybe_save_notebook_sync(state)}
  end

  @impl true
  def handle_cast({:set_notebook_attributes, client_pid, attrs}, state) do
    operation = {:set_notebook_attributes, client_pid, attrs}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:insert_section, client_pid, index}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:insert_section, client_pid, index, Utils.random_id()}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:insert_section_into, client_pid, section_id, index}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:insert_section_into, client_pid, section_id, index, Utils.random_id()}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_section_parent, client_pid, section_id, parent_id}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:set_section_parent, client_pid, section_id, parent_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:unset_section_parent, client_pid, section_id}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:unset_section_parent, client_pid, section_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:insert_cell, client_pid, section_id, index, type}, state) do
    # Include new id in the operation, so it's reproducible
    operation = {:insert_cell, client_pid, section_id, index, type, Utils.random_id()}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:delete_section, client_pid, section_id, delete_cells}, state) do
    operation = {:delete_section, client_pid, section_id, delete_cells}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:delete_cell, client_pid, cell_id}, state) do
    operation = {:delete_cell, client_pid, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:restore_cell, client_pid, cell_id}, state) do
    operation = {:restore_cell, client_pid, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:move_cell, client_pid, cell_id, offset}, state) do
    operation = {:move_cell, client_pid, cell_id, offset}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:move_section, client_pid, section_id, offset}, state) do
    operation = {:move_section, client_pid, section_id, offset}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:queue_cell_evaluation, client_pid, cell_id}, state) do
    operation = {:queue_cell_evaluation, client_pid, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:cancel_cell_evaluation, client_pid, cell_id}, state) do
    operation = {:cancel_cell_evaluation, client_pid, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_notebook_name, client_pid, name}, state) do
    operation = {:set_notebook_name, client_pid, name}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_section_name, client_pid, section_id, name}, state) do
    operation = {:set_section_name, client_pid, section_id, name}
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

  def handle_cast({:set_cell_attributes, client_pid, cell_id, attrs}, state) do
    operation = {:set_cell_attributes, client_pid, cell_id, attrs}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:connect_runtime, client_pid, runtime}, state) do
    if state.data.runtime do
      Runtime.disconnect(state.data.runtime)
    end

    runtime_monitor_ref = Runtime.connect(runtime)

    {:noreply,
     %{state | runtime_monitor_ref: runtime_monitor_ref}
     |> handle_operation({:set_runtime, client_pid, runtime})}
  end

  def handle_cast({:disconnect_runtime, client_pid}, state) do
    Runtime.disconnect(state.data.runtime)

    {:noreply,
     %{state | runtime_monitor_ref: nil}
     |> handle_operation({:set_runtime, client_pid, nil})}
  end

  def handle_cast({:set_file, client_pid, file}, state) do
    if file do
      FileGuard.lock(file, self())
    else
      :ok
    end
    |> case do
      :ok ->
        if state.data.file do
          FileGuard.unlock(state.data.file)
        end

        {:noreply, handle_operation(state, {:set_file, client_pid, file})}

      {:error, :already_in_use} ->
        broadcast_error(state.session_id, "failed to set new file because it is already in use")
        {:noreply, state}
    end
  end

  def handle_cast(:save, state) do
    {:noreply, maybe_save_notebook_async(state)}
  end

  def handle_cast(:close, state) do
    maybe_save_notebook_sync(state)
    broadcast_message(state.session_id, :session_closed)

    {:stop, :shutdown, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, _}, %{runtime_monitor_ref: ref} = state) do
    broadcast_info(state.session_id, "runtime node terminated unexpectedly")

    {:noreply,
     %{state | runtime_monitor_ref: nil}
     |> handle_operation({:set_runtime, self(), nil})}
  end

  def handle_info({:DOWN, _, :process, pid, _}, state) do
    state =
      if Map.has_key?(state.data.clients_map, pid) do
        handle_operation(state, {:client_leave, pid})
      else
        state
      end

    {:noreply, state}
  end

  def handle_info({:evaluation_output, cell_id, string}, state) do
    operation = {:add_cell_evaluation_output, self(), cell_id, string}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info({:evaluation_response, cell_id, response, metadata}, state) do
    operation = {:add_cell_evaluation_response, self(), cell_id, response, metadata}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info({:evaluation_input, cell_id, reply_to, prompt}, state) do
    input_cell = Notebook.input_cell_for_prompt(state.data.notebook, cell_id, prompt)

    reply =
      with {:ok, cell} <- input_cell,
           :ok <- Cell.Input.validate(cell) do
        {:ok, cell.value <> "\n"}
      else
        _ -> :error
      end

    send(reply_to, {:evaluation_input_reply, reply})

    state =
      case input_cell do
        {:ok, input_cell} ->
          handle_operation(state, {:bind_input, self(), cell_id, input_cell.id})

        :error ->
          state
      end

    {:noreply, state}
  end

  def handle_info({:container_down, container_ref, message}, state) do
    broadcast_error(state.session_id, "evaluation process terminated - #{message}")

    operation =
      case container_ref do
        :main_flow -> {:reflect_main_evaluation_failure, self()}
        section_id -> {:reflect_evaluation_failure, self(), section_id}
      end

    {:noreply, handle_operation(state, operation)}
  end

  def handle_info(:autosave, state) do
    {:noreply, state |> maybe_save_notebook_async() |> schedule_autosave()}
  end

  def handle_info({:user_change, user}, state) do
    operation = {:update_user, self(), user}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info({:save_finished, pid, result}, %{save_task_pid: pid} = state) do
    state = %{state | save_task_pid: nil}
    {:noreply, handle_save_finished(state, result)}
  end

  def handle_info(_message, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    cleanup_tmp_dir(state.session_id)
    :ok
  end

  # ---

  defp self_from_state(state) do
    %__MODULE__{
      id: state.session_id,
      pid: self(),
      origin: state.data.origin,
      notebook_name: state.data.notebook.name,
      file: state.data.file,
      images_dir: images_dir_from_state(state),
      created_at: state.created_at
    }
  end

  defp images_dir_from_state(%{data: %{file: nil}, session_id: id}) do
    tmp_dir = session_tmp_dir(id)
    FileSystem.File.resolve(tmp_dir, "images/")
  end

  defp images_dir_from_state(%{data: %{file: file}}) do
    images_dir_for_notebook(file)
  end

  @doc """
  Returns images directory corresponding to the given notebook file.
  """
  @spec images_dir_for_notebook(FileSystem.File.t()) :: FileSystem.File.t()
  def images_dir_for_notebook(file) do
    file
    |> FileSystem.File.containing_dir()
    |> FileSystem.File.resolve("images/")
  end

  defp session_tmp_dir(session_id) do
    tmp_dir = System.tmp_dir!() |> Path.expand()
    path = Path.join([tmp_dir, "livebook", "sessions", session_id]) <> "/"
    FileSystem.File.local(path)
  end

  defp cleanup_tmp_dir(session_id) do
    tmp_dir = session_tmp_dir(session_id)
    FileSystem.File.remove(tmp_dir)
  end

  defp copy_images(state, source) do
    images_dir = images_dir_from_state(state)

    with {:ok, source_exists?} <- FileSystem.File.exists?(source) do
      if source_exists? do
        FileSystem.File.copy(source, images_dir)
      else
        :ok
      end
    end
  end

  defp move_images(state, source) do
    images_dir = images_dir_from_state(state)

    with {:ok, source_exists?} <- FileSystem.File.exists?(source) do
      if source_exists? do
        with {:ok, destination_exists?} <- FileSystem.File.exists?(images_dir) do
          if not destination_exists? do
            # If the directory doesn't exist, we can just change
            # the directory name, which is more efficient if
            # available in the given file system
            FileSystem.File.rename(source, images_dir)
          else
            # If the directory exists, we use copy to place
            # the images there
            with :ok <- FileSystem.File.copy(source, images_dir) do
              FileSystem.File.remove(source)
            end
          end
        end
      else
        :ok
      end
    end
  end

  defp dump_images(state, images) do
    images_dir = images_dir_from_state(state)

    Enum.reduce(images, :ok, fn {filename, content}, result ->
      with :ok <- result do
        file = FileSystem.File.resolve(images_dir, filename)
        FileSystem.File.write(file, content)
      end
    end)
  end

  # Given any operation on `Livebook.Session.Data`, the process
  # does the following:
  #
  #   * broadcasts the operation to all clients immediately,
  #     so that they can update their local `Livebook.Session.Data`
  #
  #   * applies the operation to own local `Livebook.Session.Data`
  #
  #   * if necessary, performs the relevant actions (e.g. starts cell evaluation),
  #     to reflect the new `Livebook.Session.Data`
  #
  defp handle_operation(state, operation) do
    broadcast_operation(state.session_id, operation)

    case Data.apply_operation(state.data, operation) do
      {:ok, new_data, actions} ->
        %{state | data: new_data}
        |> after_operation(state, operation)
        |> handle_actions(actions)

      :error ->
        state
    end
  end

  defp after_operation(state, _prev_state, {:set_notebook_name, _pid, _name}) do
    notify_update(state)
    state
  end

  defp after_operation(state, prev_state, {:set_file, _pid, _file}) do
    prev_images_dir = images_dir_from_state(prev_state)

    if prev_state.data.file do
      copy_images(state, prev_images_dir)
    else
      move_images(state, prev_images_dir)
    end
    |> case do
      :ok ->
        :ok

      {:error, message} ->
        broadcast_error(state.session_id, "failed to copy images - #{message}")
    end

    notify_update(state)

    state
  end

  defp after_operation(
         state,
         _prev_state,
         {:set_notebook_attributes, _client_pid, %{autosave_interval_s: _}}
       ) do
    if ref = state.autosave_timer_ref do
      Process.cancel_timer(ref)
    end

    schedule_autosave(state)
  end

  defp after_operation(state, prev_state, {:client_join, _client_pid, user}) do
    unless Map.has_key?(prev_state.data.users_map, user.id) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "users:#{user.id}")
    end

    state
  end

  defp after_operation(state, prev_state, {:client_leave, client_pid}) do
    user_id = prev_state.data.clients_map[client_pid]

    unless Map.has_key?(state.data.users_map, user_id) do
      Phoenix.PubSub.unsubscribe(Livebook.PubSub, "users:#{user_id}")
    end

    state
  end

  defp after_operation(state, _prev_state, {:delete_cell, _client_pid, cell_id}) do
    entry = Enum.find(state.data.bin_entries, fn entry -> entry.cell.id == cell_id end)
    # The session LV drops cell's source, so we send them
    # the complete bin entry to override
    broadcast_message(state.session_id, {:hydrate_bin_entries, [entry]})

    state
  end

  defp after_operation(state, prev_state, {:delete_section, _client_pid, section_id, true}) do
    {:ok, section} = Notebook.fetch_section(prev_state.data.notebook, section_id)
    cell_ids = Enum.map(section.cells, & &1.id)
    entries = Enum.filter(state.data.bin_entries, fn entry -> entry.cell.id in cell_ids end)
    broadcast_message(state.session_id, {:hydrate_bin_entries, entries})

    state
  end

  defp after_operation(state, _prev_state, _operation), do: state

  defp handle_actions(state, actions) do
    Enum.reduce(actions, state, &handle_action(&2, &1))
  end

  defp handle_action(state, :start_runtime) do
    {runtime_module, args} = Livebook.Config.default_runtime()

    case apply(runtime_module, :init, args) do
      {:ok, runtime} ->
        runtime_monitor_ref = Runtime.connect(runtime)

        %{state | runtime_monitor_ref: runtime_monitor_ref}
        |> handle_operation({:set_runtime, self(), runtime})

      {:error, error} ->
        broadcast_error(state.session_id, "failed to setup runtime - #{error}")
        handle_operation(state, {:set_runtime, self(), nil})
    end
  end

  defp handle_action(state, {:start_evaluation, cell, section}) do
    path =
      case state.data.file do
        nil -> ""
        file -> file.path
      end

    file = path <> "#cell"
    opts = [file: file]

    locator = {container_ref_for_section(section), cell.id}
    prev_locator = find_prev_locator(state.data.notebook, cell, section)
    Runtime.evaluate_code(state.data.runtime, cell.source, locator, prev_locator, opts)

    evaluation_digest = :erlang.md5(cell.source)
    handle_operation(state, {:evaluation_started, self(), cell.id, evaluation_digest})
  end

  defp handle_action(state, {:stop_evaluation, section}) do
    if state.data.runtime do
      Runtime.drop_container(state.data.runtime, container_ref_for_section(section))
    end

    state
  end

  defp handle_action(state, {:forget_evaluation, cell, section}) do
    if state.data.runtime do
      Runtime.forget_evaluation(state.data.runtime, {container_ref_for_section(section), cell.id})
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
    Phoenix.PubSub.broadcast(Livebook.PubSub, "sessions:#{session_id}", message)
  end

  defp notify_update(state) do
    session = self_from_state(state)
    Livebook.Sessions.update_session(session)
    broadcast_message(state.session_id, {:session_updated, session})
  end

  defp maybe_save_notebook_async(state) do
    if should_save_notebook?(state) do
      pid = self()
      file = state.data.file
      content = LiveMarkdown.Export.notebook_to_markdown(state.data.notebook)

      {:ok, pid} =
        Task.start(fn ->
          result = FileSystem.File.write(file, content)
          send(pid, {:save_finished, self(), result})
        end)

      %{state | save_task_pid: pid}
    else
      state
    end
  end

  defp maybe_save_notebook_sync(state) do
    if should_save_notebook?(state) do
      content = LiveMarkdown.Export.notebook_to_markdown(state.data.notebook)
      result = FileSystem.File.write(state.data.file, content)
      handle_save_finished(state, result)
    else
      state
    end
  end

  defp should_save_notebook?(state) do
    state.data.file != nil and state.data.dirty and state.save_task_pid == nil
  end

  defp handle_save_finished(state, result) do
    case result do
      :ok ->
        handle_operation(state, {:mark_as_not_dirty, self()})

      {:error, message} ->
        broadcast_error(state.session_id, "failed to save notebook - #{message}")
        state
    end
  end

  @doc """
  Determines locator of the evaluation that the given
  cell depends on.
  """
  @spec find_prev_locator(Notebook.t(), Cell.t(), Section.t()) :: Runtime.locator()
  def find_prev_locator(notebook, cell, section) do
    default = {container_ref_for_section(section), nil}

    notebook
    |> Notebook.parent_cells_with_section(cell.id)
    |> Enum.find_value(default, fn {cell, section} ->
      is_struct(cell, Cell.Elixir) && {container_ref_for_section(section), cell.id}
    end)
  end

  defp container_ref_for_section(%{parent_id: nil}), do: :main_flow
  defp container_ref_for_section(section), do: section.id
end
