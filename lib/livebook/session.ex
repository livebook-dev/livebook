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
  # In practice, every evaluation container is a `Livebook.Runtime.Evaluator`
  # process, so we have one such process for the main flow and one
  # for each branching section. Since a branching section inherits
  # the evaluation context from the parent section, the last context
  # needs to be copied from the main flow evaluator to the branching
  # section evaluator. The latter synchronously asks the former for
  # that context using `Livebook.Runtime.Evaluator.fetch_evaluation_context/3`.
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
  defstruct [
    :id,
    :pid,
    :origin,
    :notebook_name,
    :file,
    :images_dir,
    :created_at,
    :memory_usage
  ]

  use GenServer, restart: :temporary

  alias Livebook.Session.{Data, FileGuard}
  alias Livebook.{Utils, Notebook, Delta, Runtime, LiveMarkdown, FileSystem}
  alias Livebook.Users.User
  alias Livebook.Notebook.{Cell, Section}

  @timeout :infinity
  @main_container_ref :main_flow
  @client_id "__server__"
  @anonymous_client_id "__anonymous__"

  @type t :: %__MODULE__{
          id: id(),
          pid: pid(),
          origin: Notebook.ContentLoader.location() | nil,
          notebook_name: String.t(),
          file: FileSystem.File.t() | nil,
          images_dir: FileSystem.File.t(),
          created_at: DateTime.t(),
          memory_usage: memory_usage()
        }

  @type state :: %{
          session_id: id(),
          data: Data.t(),
          created_at: DateTime.t(),
          runtime_monitor_ref: reference() | nil,
          autosave_timer_ref: reference() | nil,
          save_task_pid: pid() | nil,
          saved_default_file: FileSystem.File.t() | nil,
          memory_usage: memory_usage()
        }

  @type memory_usage ::
          %{
            runtime: Livebook.Runtime.runtime_memory() | nil,
            system: Livebook.SystemResources.memory()
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

    * `:autosave_path` - a local directory to save notebooks without a file into.
      Defaults to `Livebook.Settings.autosave_path/0`
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
    GenServer.call(pid, :describe_self, @timeout)
  end

  @doc """
  Registers a session client, so that the session is aware of it.

  The client process is automatically unregistered when it terminates.

  Returns the current session data, which the client can than
  keep in sync with the server by subscribing to the `sessions:id`
  topic and receiving operations to apply.

  Also returns a unique client identifier representing the registered
  client.
  """
  @spec register_client(pid(), pid(), User.t()) :: {Data.t(), Data.client_id()}
  def register_client(pid, client_pid, user) do
    GenServer.call(pid, {:register_client, client_pid, user}, @timeout)
  end

  @doc """
  Returns data of the given session.
  """
  @spec get_data(pid()) :: Data.t()
  def get_data(pid) do
    GenServer.call(pid, :get_data, @timeout)
  end

  @doc """
  Returns the current notebook structure.
  """
  @spec get_notebook(pid()) :: Notebook.t()
  def get_notebook(pid) do
    GenServer.call(pid, :get_notebook, @timeout)
  end

  @doc """
  Subscribes to session messages.

  ## Messages

    * `:session_closed`
    * `{:session_updated, session}`
    * `{:hydrate_bin_entries, entries}`
    * `{:operation, operation}`
    * `{:error, error}`

  """
  @spec subscribe(id()) :: :ok | {:error, term()}
  def subscribe(session_id) do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
  end

  @doc """
  Computes the file name for download.

  Note that the name doesn't have any extension.

  If the notebook has an associated file, the same name is used,
  otherwise it is computed from the notebook title.
  """
  @spec file_name_for_download(t()) :: String.t()
  def file_name_for_download(session)

  def file_name_for_download(%{file: nil} = session) do
    suggested_filename(session, nil)
  end

  def file_name_for_download(session) do
    session.file
    |> FileSystem.File.name()
    |> Path.rootname()
  end

  @doc """
  Fetches assets matching the given hash.

  The assets are cached locally and fetched from the runtime
  only once.

  See `local_asset_path/2` for locating a specific asset.
  """
  @spec fetch_assets(pid(), String.t()) :: :ok | {:error, String.t()}
  def fetch_assets(pid, hash) do
    local_assets_path = local_assets_path(hash)

    if non_empty_dir?(local_assets_path) do
      :ok
    else
      with {:ok, runtime, archive_path} <-
             GenServer.call(pid, {:get_runtime_and_archive_path, hash}, @timeout) do
        fun = fn ->
          # Make sure the file hasn't been fetched by this point
          unless non_empty_dir?(local_assets_path) do
            {:ok, archive_binary} = Runtime.read_file(runtime, archive_path)
            extract_archive!(archive_binary, local_assets_path)
          end
        end

        # Fetch assets in a separate process and avoid several
        # simultaneous fateches of the same assets
        case Livebook.Utils.UniqueTask.run(hash, fun) do
          :ok -> :ok
          :error -> {:error, "failed to fetch assets"}
        end
      end
    end
  end

  defp non_empty_dir?(path) do
    match?({:ok, [_ | _]}, File.ls(path))
  end

  @doc """
  Sends notebook attributes update to the server.
  """
  @spec set_notebook_attributes(pid(), map()) :: :ok
  def set_notebook_attributes(pid, attrs) do
    GenServer.cast(pid, {:set_notebook_attributes, self(), attrs})
  end

  @doc """
  Sends section insertion request to the server.
  """
  @spec insert_section(pid(), non_neg_integer()) :: :ok
  def insert_section(pid, index) do
    GenServer.cast(pid, {:insert_section, self(), index})
  end

  @doc """
  Sends section insertion request to the server.
  """
  @spec insert_section_into(pid(), Section.id(), non_neg_integer()) :: :ok
  def insert_section_into(pid, section_id, index) do
    GenServer.cast(pid, {:insert_section_into, self(), section_id, index})
  end

  @doc """
  Sends parent update request to the server.
  """
  @spec set_section_parent(pid(), Section.id(), Section.id()) :: :ok
  def set_section_parent(pid, section_id, parent_id) do
    GenServer.cast(pid, {:set_section_parent, self(), section_id, parent_id})
  end

  @doc """
  Sends parent update request to the server.
  """
  @spec unset_section_parent(pid(), Section.id()) :: :ok
  def unset_section_parent(pid, section_id) do
    GenServer.cast(pid, {:unset_section_parent, self(), section_id})
  end

  @doc """
  Sends cell insertion request to the server.
  """
  @spec insert_cell(pid(), Section.id(), non_neg_integer(), Cell.type(), map()) :: :ok
  def insert_cell(pid, section_id, index, type, attrs \\ %{}) do
    GenServer.cast(pid, {:insert_cell, self(), section_id, index, type, attrs})
  end

  @doc """
  Sends section deletion request to the server.
  """
  @spec delete_section(pid(), Section.id(), boolean()) :: :ok
  def delete_section(pid, section_id, delete_cells) do
    GenServer.cast(pid, {:delete_section, self(), section_id, delete_cells})
  end

  @doc """
  Sends cell deletion request to the server.
  """
  @spec delete_cell(pid(), Cell.id()) :: :ok
  def delete_cell(pid, cell_id) do
    GenServer.cast(pid, {:delete_cell, self(), cell_id})
  end

  @doc """
  Sends cell restoration request to the server.
  """
  @spec restore_cell(pid(), Cell.id()) :: :ok
  def restore_cell(pid, cell_id) do
    GenServer.cast(pid, {:restore_cell, self(), cell_id})
  end

  @doc """
  Sends cell move request to the server.
  """
  @spec move_cell(pid(), Cell.id(), integer()) :: :ok
  def move_cell(pid, cell_id, offset) do
    GenServer.cast(pid, {:move_cell, self(), cell_id, offset})
  end

  @doc """
  Sends section move request to the server.
  """
  @spec move_section(pid(), Section.id(), integer()) :: :ok
  def move_section(pid, section_id, offset) do
    GenServer.cast(pid, {:move_section, self(), section_id, offset})
  end

  @doc """
  Sends cell recover request to the server.
  """
  @spec recover_smart_cell(pid(), Cell.id()) :: :ok
  def recover_smart_cell(pid, cell_id) do
    GenServer.cast(pid, {:recover_smart_cell, self(), cell_id})
  end

  @doc """
  Sends cell conversion request to the server.
  """
  @spec convert_smart_cell(pid(), Cell.id()) :: :ok
  def convert_smart_cell(pid, cell_id) do
    GenServer.cast(pid, {:convert_smart_cell, self(), cell_id})
  end

  @doc """
  Sends dependencies addition request to the server.
  """
  @spec add_dependencies(pid(), list(Runtime.dependency())) :: :ok
  def add_dependencies(pid, dependencies) do
    GenServer.cast(pid, {:add_dependencies, dependencies})
  end

  @doc """
  Sends cell evaluation request to the server.
  """
  @spec queue_cell_evaluation(pid(), Cell.id()) :: :ok
  def queue_cell_evaluation(pid, cell_id) do
    GenServer.cast(pid, {:queue_cell_evaluation, self(), cell_id})
  end

  @doc """
  Sends section evaluation request to the server.
  """
  @spec queue_section_evaluation(pid(), Section.id()) :: :ok
  def queue_section_evaluation(pid, section_id) do
    GenServer.cast(pid, {:queue_section_evaluation, self(), section_id})
  end

  @doc """
  Sends input bound cells evaluation request to the server.
  """
  @spec queue_bound_cells_evaluation(pid(), Data.input_id()) :: :ok
  def queue_bound_cells_evaluation(pid, input_id) do
    GenServer.cast(pid, {:queue_bound_cells_evaluation, self(), input_id})
  end

  @doc """
  Sends full evaluation request to the server.

  All outdated (new/stale/changed) cells, as well as cells given
  as `forced_cell_ids` are scheduled for evaluation.
  """
  @spec queue_full_evaluation(pid(), list(Cell.id())) :: :ok
  def queue_full_evaluation(pid, forced_cell_ids) do
    GenServer.cast(pid, {:queue_full_evaluation, self(), forced_cell_ids})
  end

  @doc """
  Sends reevaluation request to the server.

  Schedules evaluation of all cells that have been evaluated
  previously, until the first fresh cell.
  """
  @spec queue_cells_reevaluation(pid()) :: :ok
  def queue_cells_reevaluation(pid) do
    GenServer.cast(pid, {:queue_cells_reevaluation, self()})
  end

  @doc """
  Sends cell evaluation cancellation request to the server.
  """
  @spec cancel_cell_evaluation(pid(), Cell.id()) :: :ok
  def cancel_cell_evaluation(pid, cell_id) do
    GenServer.cast(pid, {:cancel_cell_evaluation, self(), cell_id})
  end

  @doc """
  Sends erase outputs request to the server.
  """
  @spec erase_outputs(pid()) :: :ok
  def erase_outputs(pid) do
    GenServer.cast(pid, {:erase_outputs, self()})
  end

  @doc """
  Sends notebook name update request to the server.
  """
  @spec set_notebook_name(pid(), String.t()) :: :ok
  def set_notebook_name(pid, name) do
    GenServer.cast(pid, {:set_notebook_name, self(), name})
  end

  @doc """
  Sends section name update request to the server.
  """
  @spec set_section_name(pid(), Section.id(), String.t()) :: :ok
  def set_section_name(pid, section_id, name) do
    GenServer.cast(pid, {:set_section_name, self(), section_id, name})
  end

  @doc """
  Sends a cell delta to apply to the server.
  """
  @spec apply_cell_delta(
          pid(),
          Cell.id(),
          Data.cell_source_tag(),
          Delta.t(),
          Data.cell_revision()
        ) :: :ok
  def apply_cell_delta(pid, cell_id, tag, delta, revision) do
    GenServer.cast(pid, {:apply_cell_delta, self(), cell_id, tag, delta, revision})
  end

  @doc """
  Informs at what revision the given client is.

  This helps to remove old deltas that are no longer necessary.
  """
  @spec report_cell_revision(
          pid(),
          Cell.id(),
          Data.cell_source_tag(),
          Data.cell_revision()
        ) :: :ok
  def report_cell_revision(pid, cell_id, tag, revision) do
    GenServer.cast(pid, {:report_cell_revision, self(), cell_id, tag, revision})
  end

  @doc """
  Sends a cell attributes update to the server.
  """
  @spec set_cell_attributes(pid(), Cell.id(), map()) :: :ok
  def set_cell_attributes(pid, cell_id, attrs) do
    GenServer.cast(pid, {:set_cell_attributes, self(), cell_id, attrs})
  end

  @doc """
  Sends a input value update to the server.
  """
  @spec set_input_value(pid(), Data.input_id(), term()) :: :ok
  def set_input_value(pid, input_id, value) do
    GenServer.cast(pid, {:set_input_value, self(), input_id, value})
  end

  @doc """
  Sends runtime update to the server.

  If the runtime is connected, the session takes the ownership.
  """
  @spec set_runtime(pid(), Runtime.t()) :: :ok
  def set_runtime(pid, runtime) do
    GenServer.cast(pid, {:set_runtime, self(), runtime})
  end

  @doc """
  Sends file location update request to the server.
  """
  @spec set_file(pid(), FileSystem.File.t() | nil) :: :ok
  def set_file(pid, file) do
    GenServer.cast(pid, {:set_file, self(), file})
  end

  @doc """
  Sends a secret addition request to the server.
  """
  @spec put_secret(pid(), map()) :: :ok
  def put_secret(pid, secret) do
    GenServer.cast(pid, {:put_secret, self(), secret})
  end

  @doc """
  Sends save request to the server.

  If there's a file set and the notebook changed since the last save,
  it will be persisted to said file.

  Note that notebooks are automatically persisted every @autosave_interval
  milliseconds.
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
    GenServer.call(pid, :save_sync, @timeout)
  end

  @doc """
  Closes one or more sessions.

  This results in saving the file and broadcasting
  a :closed message to the session topic.
  """
  @spec close(pid() | [pid()]) :: :ok
  def close(pid) do
    _ = call_many(List.wrap(pid), :close)
    Livebook.SystemResources.update()
    :ok
  end

  @doc """
  Disconnects one or more sessions from the current runtime.

  Note that this results in clearing the evaluation state.
  """
  @spec disconnect_runtime(pid() | [pid()]) :: :ok
  def disconnect_runtime(pid) do
    _ = call_many(List.wrap(pid), {:disconnect_runtime, self()})
    Livebook.SystemResources.update()
    :ok
  end

  defp call_many(list, request) do
    list
    |> Enum.map(&:gen_server.send_request(&1, request))
    |> Enum.map(&:gen_server.wait_response(&1, :infinity))
  end

  ## Callbacks

  @impl true
  def init(opts) do
    id = Keyword.fetch!(opts, :id)

    {:ok, worker_pid} = Livebook.Session.Worker.start_link(id)

    with {:ok, state} <- init_state(id, worker_pid, opts),
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

  defp init_state(id, worker_pid, opts) do
    with {:ok, data} <- init_data(opts) do
      state = %{
        session_id: id,
        data: data,
        client_pids_with_id: %{},
        created_at: DateTime.utc_now(),
        runtime_monitor_ref: nil,
        autosave_timer_ref: nil,
        autosave_path: opts[:autosave_path],
        save_task_pid: nil,
        saved_default_file: nil,
        memory_usage: %{runtime: nil, system: Livebook.SystemResources.memory()},
        worker_pid: worker_pid
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
    %{Notebook.new() | sections: [%{Section.new() | cells: [Cell.new(:code)]}]}
  end

  defp schedule_autosave(state) do
    if interval_s = state.data.notebook.autosave_interval_s do
      ref = Process.send_after(self(), :autosave, interval_s * 1000)
      %{state | autosave_timer_ref: ref}
    else
      %{state | autosave_timer_ref: nil}
    end
  end

  defp unschedule_autosave(%{autosave_timer_ref: nil} = state), do: state

  defp unschedule_autosave(state) do
    if Process.cancel_timer(state.autosave_timer_ref) == false do
      receive do
        :autosave -> :ok
      end
    end

    %{state | autosave_timer_ref: nil}
  end

  @impl true
  def handle_call(:describe_self, _from, state) do
    {:reply, self_from_state(state), state}
  end

  def handle_call({:register_client, client_pid, user}, _from, state) do
    {state, client_id} =
      if client_id = state.client_pids_with_id[client_pid] do
        {state, client_id}
      else
        Process.monitor(client_pid)
        client_id = Utils.random_id()
        state = handle_operation(state, {:client_join, client_id, user})
        state = put_in(state.client_pids_with_id[client_pid], client_id)
        {state, client_id}
      end

    {:reply, {state.data, client_id}, state}
  end

  def handle_call(:get_data, _from, state) do
    {:reply, state.data, state}
  end

  def handle_call({:get_runtime_and_archive_path, hash}, _from, state) do
    assets_info = Notebook.find_asset_info(state.data.notebook, hash)
    runtime = state.data.runtime

    reply =
      cond do
        assets_info == nil ->
          {:error, "unknown hash"}

        not Runtime.connected?(runtime) ->
          {:error, "runtime not started"}

        true ->
          {:ok, runtime, assets_info.archive_path}
      end

    {:reply, reply, state}
  end

  def handle_call(:get_notebook, _from, state) do
    {:reply, state.data.notebook, state}
  end

  def handle_call(:save_sync, _from, state) do
    {:reply, :ok, maybe_save_notebook_sync(state)}
  end

  def handle_call(:close, _from, state) do
    maybe_save_notebook_sync(state)
    broadcast_message(state.session_id, :session_closed)

    {:stop, :shutdown, :ok, state}
  end

  def handle_call({:disconnect_runtime, client_pid}, _from, state) do
    client_id = client_id(state, client_pid)

    state =
      if Runtime.connected?(state.data.runtime) do
        {:ok, runtime} = Runtime.disconnect(state.data.runtime)

        %{state | runtime_monitor_ref: nil}
        |> handle_operation({:set_runtime, client_id, runtime})
      else
        state
      end

    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:set_notebook_attributes, client_pid, attrs}, state) do
    client_id = client_id(state, client_pid)
    operation = {:set_notebook_attributes, client_id, attrs}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:insert_section, client_pid, index}, state) do
    client_id = client_id(state, client_pid)
    # Include new id in the operation, so it's reproducible
    operation = {:insert_section, client_id, index, Utils.random_id()}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:insert_section_into, client_pid, section_id, index}, state) do
    client_id = client_id(state, client_pid)
    # Include new id in the operation, so it's reproducible
    operation = {:insert_section_into, client_id, section_id, index, Utils.random_id()}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_section_parent, client_pid, section_id, parent_id}, state) do
    client_id = client_id(state, client_pid)
    # Include new id in the operation, so it's reproducible
    operation = {:set_section_parent, client_id, section_id, parent_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:unset_section_parent, client_pid, section_id}, state) do
    client_id = client_id(state, client_pid)
    # Include new id in the operation, so it's reproducible
    operation = {:unset_section_parent, client_id, section_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:insert_cell, client_pid, section_id, index, type, attrs}, state) do
    client_id = client_id(state, client_pid)
    # Include new id in the operation, so it's reproducible
    operation = {:insert_cell, client_id, section_id, index, type, Utils.random_id(), attrs}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:delete_section, client_pid, section_id, delete_cells}, state) do
    client_id = client_id(state, client_pid)
    operation = {:delete_section, client_id, section_id, delete_cells}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:delete_cell, client_pid, cell_id}, state) do
    client_id = client_id(state, client_pid)
    operation = {:delete_cell, client_id, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:restore_cell, client_pid, cell_id}, state) do
    client_id = client_id(state, client_pid)
    operation = {:restore_cell, client_id, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:move_cell, client_pid, cell_id, offset}, state) do
    client_id = client_id(state, client_pid)
    operation = {:move_cell, client_id, cell_id, offset}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:move_section, client_pid, section_id, offset}, state) do
    client_id = client_id(state, client_pid)
    operation = {:move_section, client_id, section_id, offset}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:recover_smart_cell, client_pid, cell_id}, state) do
    client_id = client_id(state, client_pid)
    operation = {:recover_smart_cell, client_id, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:convert_smart_cell, client_pid, cell_id}, state) do
    client_id = client_id(state, client_pid)

    state =
      with {:ok, %Cell.Smart{} = cell, section} <-
             Notebook.fetch_cell_and_section(state.data.notebook, cell_id) do
        index = Enum.find_index(section.cells, &(&1 == cell))

        attrs = Map.take(cell, [:source, :outputs])

        state
        |> handle_operation({:delete_cell, client_id, cell.id})
        |> handle_operation(
          {:insert_cell, client_id, section.id, index, :code, Utils.random_id(), attrs}
        )
      else
        _ -> state
      end

    {:noreply, state}
  end

  def handle_cast({:add_dependencies, dependencies}, state) do
    {:noreply, do_add_dependencies(state, dependencies)}
  end

  def handle_cast({:queue_cell_evaluation, client_pid, cell_id}, state) do
    client_id = client_id(state, client_pid)
    operation = {:queue_cells_evaluation, client_id, [cell_id]}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:queue_section_evaluation, client_pid, section_id}, state) do
    client_id = client_id(state, client_pid)

    case Notebook.fetch_section(state.data.notebook, section_id) do
      {:ok, section} ->
        cell_ids = for cell <- section.cells, Cell.evaluable?(cell), do: cell.id
        operation = {:queue_cells_evaluation, client_id, cell_ids}
        {:noreply, handle_operation(state, operation)}

      :error ->
        {:noreply, state}
    end
  end

  def handle_cast({:queue_bound_cells_evaluation, client_pid, input_id}, state) do
    client_id = client_id(state, client_pid)

    cell_ids =
      for {bound_cell, _} <- Data.bound_cells_with_section(state.data, input_id),
          do: bound_cell.id

    operation = {:queue_cells_evaluation, client_id, cell_ids}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:queue_full_evaluation, client_pid, forced_cell_ids}, state) do
    client_id = client_id(state, client_pid)

    cell_ids = Data.cell_ids_for_full_evaluation(state.data, forced_cell_ids)

    operation = {:queue_cells_evaluation, client_id, cell_ids}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:queue_cells_reevaluation, client_pid}, state) do
    client_id = client_id(state, client_pid)

    cell_ids = Data.cell_ids_for_reevaluation(state.data)

    operation = {:queue_cells_evaluation, client_id, cell_ids}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:cancel_cell_evaluation, client_pid, cell_id}, state) do
    client_id = client_id(state, client_pid)
    operation = {:cancel_cell_evaluation, client_id, cell_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:erase_outputs, client_pid}, state) do
    client_id = client_id(state, client_pid)
    operation = {:erase_outputs, client_id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_notebook_name, client_pid, name}, state) do
    client_id = client_id(state, client_pid)
    operation = {:set_notebook_name, client_id, name}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_section_name, client_pid, section_id, name}, state) do
    client_id = client_id(state, client_pid)
    operation = {:set_section_name, client_id, section_id, name}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:apply_cell_delta, client_pid, cell_id, tag, delta, revision}, state) do
    client_id = client_id(state, client_pid)
    operation = {:apply_cell_delta, client_id, cell_id, tag, delta, revision}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:report_cell_revision, client_pid, cell_id, tag, revision}, state) do
    client_id = client_id(state, client_pid)
    operation = {:report_cell_revision, client_id, cell_id, tag, revision}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_cell_attributes, client_pid, cell_id, attrs}, state) do
    client_id = client_id(state, client_pid)
    operation = {:set_cell_attributes, client_id, cell_id, attrs}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_input_value, client_pid, input_id, value}, state) do
    client_id = client_id(state, client_pid)
    operation = {:set_input_value, client_id, input_id, value}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast({:set_runtime, client_pid, runtime}, state) do
    client_id = client_id(state, client_pid)

    if Runtime.connected?(state.data.runtime) do
      {:ok, _} = Runtime.disconnect(state.data.runtime)
    end

    state =
      if Runtime.connected?(runtime) do
        own_runtime(runtime, state)
      else
        state
      end

    {:noreply, handle_operation(state, {:set_runtime, client_id, runtime})}
  end

  def handle_cast({:set_file, client_pid, file}, state) do
    client_id = client_id(state, client_pid)

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

        {:noreply, handle_operation(state, {:set_file, client_id, file})}

      {:error, :already_in_use} ->
        broadcast_error(state.session_id, "failed to set new file because it is already in use")
        {:noreply, state}
    end
  end

  def handle_cast({:put_secret, client_pid, secret}, state) do
    client_id = client_id(state, client_pid)
    operation = {:put_secret, client_id, secret}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_cast(:save, state) do
    {:noreply, maybe_save_notebook_async(state)}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, reason}, %{runtime_monitor_ref: ref} = state) do
    broadcast_error(
      state.session_id,
      "runtime node terminated unexpectedly - #{Exception.format_exit(reason)}"
    )

    {:noreply,
     %{state | runtime_monitor_ref: nil}
     |> handle_operation(
       {:set_runtime, @client_id, Livebook.Runtime.duplicate(state.data.runtime)}
     )}
  end

  def handle_info({:DOWN, _, :process, pid, _}, state) do
    state =
      if client_id = state.client_pids_with_id[pid] do
        handle_operation(state, {:client_leave, client_id})
      else
        state
      end

    {:noreply, state}
  end

  def handle_info({:runtime_evaluation_output, cell_id, output}, state) do
    operation = {:add_cell_evaluation_output, @client_id, cell_id, output}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info({:runtime_evaluation_response, cell_id, response, metadata}, state) do
    {memory_usage, metadata} = Map.pop(metadata, :memory_usage)
    operation = {:add_cell_evaluation_response, @client_id, cell_id, response, metadata}

    {:noreply,
     state
     |> put_memory_usage(memory_usage)
     |> handle_operation(operation)
     |> notify_update()}
  end

  def handle_info({:runtime_evaluation_input, cell_id, reply_to, input_id}, state) do
    {reply, state} =
      with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(state.data.notebook, cell_id),
           {:ok, value} <- Map.fetch(state.data.input_values, input_id) do
        state = handle_operation(state, {:bind_input, @client_id, cell.id, input_id})
        {{:ok, value}, state}
      else
        _ -> {:error, state}
      end

    send(reply_to, {:runtime_evaluation_input_reply, reply})

    {:noreply, state}
  end

  def handle_info({:runtime_container_down, container_ref, message}, state) do
    broadcast_error(state.session_id, "evaluation process terminated - #{message}")

    operation =
      case container_ref do
        @main_container_ref -> {:reflect_main_evaluation_failure, @client_id}
        section_id -> {:reflect_evaluation_failure, @client_id, section_id}
      end

    {:noreply, handle_operation(state, operation)}
  end

  def handle_info(:autosave, state) do
    {:noreply, state |> maybe_save_notebook_async() |> schedule_autosave()}
  end

  def handle_info({:user_change, user}, state) do
    operation = {:update_user, @client_id, user}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info({:save_finished, pid, result, file, default?}, %{save_task_pid: pid} = state) do
    state = %{state | save_task_pid: nil}
    {:noreply, handle_save_finished(state, result, file, default?)}
  end

  def handle_info({:runtime_memory_usage, runtime_memory}, state) do
    {:noreply, state |> put_memory_usage(runtime_memory) |> notify_update()}
  end

  def handle_info({:runtime_smart_cell_definitions, definitions}, state) do
    operation = {:set_smart_cell_definitions, @client_id, definitions}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info({:runtime_smart_cell_started, id, info}, state) do
    info =
      if info.editor do
        normalize_newlines = &String.replace(&1, "\r\n", "\n")
        info = update_in(info.source, normalize_newlines)
        update_in(info.editor.source, normalize_newlines)
      else
        info
      end

    case Notebook.fetch_cell_and_section(state.data.notebook, id) do
      {:ok, cell, _section} ->
        delta = Livebook.JSInterop.diff(cell.source, info.source)
        operation = {:smart_cell_started, @client_id, id, delta, info.js_view, info.editor}
        {:noreply, handle_operation(state, operation)}

      :error ->
        {:noreply, state}
    end
  end

  def handle_info({:runtime_smart_cell_down, id}, state) do
    operation = {:smart_cell_down, @client_id, id}
    {:noreply, handle_operation(state, operation)}
  end

  def handle_info({:runtime_smart_cell_update, id, attrs, source, info}, state) do
    case Notebook.fetch_cell_and_section(state.data.notebook, id) do
      {:ok, cell, _section} ->
        delta = Livebook.JSInterop.diff(cell.source, source)
        operation = {:update_smart_cell, @client_id, id, attrs, delta, info.reevaluate}
        {:noreply, handle_operation(state, operation)}

      :error ->
        {:noreply, state}
    end
  end

  def handle_info({:pong, {:smart_cell_evaluation, cell_id}, _info}, state) do
    state =
      with {:ok, cell, section} <- Notebook.fetch_cell_and_section(state.data.notebook, cell_id),
           :evaluating <- state.data.cell_infos[cell.id].eval.status do
        start_evaluation(state, cell, section)
      else
        _ -> state
      end

    {:noreply, state}
  end

  def handle_info(_message, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    cleanup_tmp_dir(state.session_id)
    :ok
  end

  # ---

  defp client_id(state, client_pid) do
    state.client_pids_with_id[client_pid] || @anonymous_client_id
  end

  defp self_from_state(state) do
    %__MODULE__{
      id: state.session_id,
      pid: self(),
      origin: state.data.origin,
      notebook_name: state.data.notebook.name,
      file: state.data.file,
      images_dir: images_dir_from_state(state),
      created_at: state.created_at,
      memory_usage: state.memory_usage
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
    livebook_tmp_path()
    |> Path.join("sessions/#{session_id}")
    |> FileSystem.Utils.ensure_dir_path()
    |> FileSystem.File.local()
  end

  defp cleanup_tmp_dir(session_id) do
    tmp_dir = session_tmp_dir(session_id)
    FileSystem.File.remove(tmp_dir)
  end

  @doc """
  Returns a local path to the directory for all assets for hash.
  """
  def local_assets_path(hash) do
    Path.join([livebook_tmp_path(), "assets", encode_path_component(hash)])
  end

  @doc """
  Returns a local path to asset matching the given
  hash and path.

  The file is not guaranteed to exist. See `fetch_assets/2`
  for fetching assets through a particular session.

  The path is expected to be a simple relative path
  within the assets directory, otherwise an error is
  returned.
  """
  @spec local_asset_path(String.t(), String.t()) :: {:ok, String.t()} | :error
  def local_asset_path(hash, asset_path) do
    assets_path = local_assets_path(hash)
    local_asset_path = Path.expand(asset_path, assets_path)

    if String.starts_with?(local_asset_path, assets_path <> "/") do
      {:ok, local_asset_path}
    else
      :error
    end
  end

  defp encode_path_component(component) do
    String.replace(component, [".", "/", "\\", ":"], "_")
  end

  defp livebook_tmp_path() do
    tmp_dir = System.tmp_dir!() |> Path.expand()
    Path.join(tmp_dir, "livebook")
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
          if destination_exists? do
            # If the directory exists, we use copy to place
            # the images there
            with :ok <- FileSystem.File.copy(source, images_dir) do
              FileSystem.File.remove(source)
            end
          else
            # If the directory doesn't exist, we can just change
            # the directory name, which is more efficient if
            # available in the given file system
            FileSystem.File.rename(source, images_dir)
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

  defp own_runtime(runtime, state) do
    runtime_monitor_ref = Runtime.take_ownership(runtime, runtime_broadcast_to: state.worker_pid)
    %{state | runtime_monitor_ref: runtime_monitor_ref}
  end

  defp do_add_dependencies(state, dependencies) do
    {:ok, cell, _} = Notebook.fetch_cell_and_section(state.data.notebook, Cell.setup_cell_id())
    source = cell.source

    case Runtime.add_dependencies(state.data.runtime, source, dependencies) do
      {:ok, ^source} ->
        state

      {:ok, new_source} ->
        delta = Livebook.JSInterop.diff(cell.source, new_source)
        revision = state.data.cell_infos[cell.id].sources.primary.revision + 1

        handle_operation(
          state,
          {:apply_cell_delta, @client_id, cell.id, :primary, delta, revision}
        )

      {:error, message} ->
        broadcast_error(
          state.session_id,
          "failed to add dependencies to the setup cell, reason:\n\n#{message}"
        )

        state
    end
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

  defp after_operation(state, _prev_state, {:set_notebook_name, _client_id, _name}) do
    notify_update(state)
  end

  defp after_operation(state, _prev_state, {:set_runtime, _client_id, runtime}) do
    if Runtime.connected?(runtime) do
      set_runtime_secrets(state, state.data.secrets)
      state
    else
      state
      |> put_memory_usage(nil)
      |> notify_update()
    end
  end

  defp after_operation(state, prev_state, {:set_file, _client_id, _file}) do
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
  end

  defp after_operation(
         state,
         _prev_state,
         {:set_notebook_attributes, _client_id, %{autosave_interval_s: _}}
       ) do
    state
    |> unschedule_autosave()
    |> schedule_autosave()
  end

  defp after_operation(state, prev_state, {:client_join, _client_id, user}) do
    unless Map.has_key?(prev_state.data.users_map, user.id) do
      Livebook.Users.subscribe(user.id)
    end

    state
  end

  defp after_operation(state, prev_state, {:client_leave, client_id}) do
    user_id = prev_state.data.clients_map[client_id]

    unless Map.has_key?(state.data.users_map, user_id) do
      Livebook.Users.unsubscribe(user_id)
    end

    state
  end

  defp after_operation(state, _prev_state, {:delete_cell, _client_id, cell_id}) do
    entry = Enum.find(state.data.bin_entries, fn entry -> entry.cell.id == cell_id end)
    # The session LV drops cell's source, so we send them
    # the complete bin entry to override
    broadcast_message(state.session_id, {:hydrate_bin_entries, [entry]})

    state
  end

  defp after_operation(state, prev_state, {:delete_section, _client_id, section_id, true}) do
    {:ok, section} = Notebook.fetch_section(prev_state.data.notebook, section_id)
    cell_ids = Enum.map(section.cells, & &1.id)
    entries = Enum.filter(state.data.bin_entries, fn entry -> entry.cell.id in cell_ids end)
    broadcast_message(state.session_id, {:hydrate_bin_entries, entries})

    state
  end

  defp after_operation(
         state,
         _prev_state,
         {:apply_cell_delta, _client_id, cell_id, tag, _delta, _revision}
       ) do
    with :secondary <- tag,
         {:ok, %Cell.Smart{} = cell, _section} <-
           Notebook.fetch_cell_and_section(state.data.notebook, cell_id) do
      send(cell.js_view.pid, {:editor_source, cell.editor.source})
    end

    state
  end

  defp after_operation(state, _prev_state, {:put_secret, _client_id, secret}) do
    if Runtime.connected?(state.data.runtime), do: set_runtime_secrets(state, [secret])
    state
  end

  defp after_operation(state, _prev_state, _operation), do: state

  defp handle_actions(state, actions) do
    Enum.reduce(actions, state, &handle_action(&2, &1))
  end

  defp handle_action(state, :connect_runtime) do
    case Runtime.connect(state.data.runtime) do
      {:ok, runtime} ->
        state = own_runtime(runtime, state)
        handle_operation(state, {:set_runtime, @client_id, runtime})

      {:error, error} ->
        broadcast_error(state.session_id, "failed to connect runtime - #{error}")
        handle_operation(state, {:set_runtime, @client_id, state.data.runtime})
    end
  end

  defp handle_action(state, {:start_evaluation, cell, section}) do
    info = state.data.cell_infos[cell.id]

    if is_struct(cell, Cell.Smart) and info.status == :started do
      # We do a ping and start evaluation only once we get a reply,
      # this way we make sure we received all relevant source changes
      send(
        cell.js_view.pid,
        {:ping, self(), {:smart_cell_evaluation, cell.id}, %{ref: cell.js_view.ref}}
      )

      state
    else
      start_evaluation(state, cell, section)
    end
  end

  defp handle_action(state, {:stop_evaluation, section}) do
    if Runtime.connected?(state.data.runtime) do
      Runtime.drop_container(state.data.runtime, container_ref_for_section(section))
    end

    state
  end

  defp handle_action(state, {:forget_evaluation, cell, section}) do
    if Runtime.connected?(state.data.runtime) do
      Runtime.forget_evaluation(state.data.runtime, {container_ref_for_section(section), cell.id})
    end

    state
  end

  defp handle_action(state, {:start_smart_cell, cell, section}) do
    if Runtime.connected?(state.data.runtime) do
      base_locator = find_base_locator(state.data, cell, section, existing: true)
      Runtime.start_smart_cell(state.data.runtime, cell.kind, cell.id, cell.attrs, base_locator)
    end

    state
  end

  defp handle_action(state, {:set_smart_cell_base, cell, section, parent}) do
    if Runtime.connected?(state.data.runtime) do
      base_locator =
        case parent do
          nil ->
            {container_ref_for_section(section), nil}

          {parent_cell, parent_section} ->
            {container_ref_for_section(parent_section), parent_cell.id}
        end

      Runtime.set_smart_cell_base_locator(state.data.runtime, cell.id, base_locator)
    end

    state
  end

  defp handle_action(state, {:stop_smart_cell, cell}) do
    if Runtime.connected?(state.data.runtime) do
      Runtime.stop_smart_cell(state.data.runtime, cell.id)
    end

    state
  end

  defp handle_action(state, _action), do: state

  defp start_evaluation(state, cell, section) do
    path =
      case state.data.file do
        nil -> ""
        file -> file.path
      end

    file = path <> "#cell:#{cell.id}"

    smart_cell_ref =
      case cell do
        %Cell.Smart{} -> cell.id
        _ -> nil
      end

    opts = [file: file, smart_cell_ref: smart_cell_ref]

    locator = {container_ref_for_section(section), cell.id}
    base_locator = find_base_locator(state.data, cell, section)
    Runtime.evaluate_code(state.data.runtime, cell.source, locator, base_locator, opts)

    evaluation_digest = :erlang.md5(cell.source)
    handle_operation(state, {:evaluation_started, @client_id, cell.id, evaluation_digest})
  end

  defp broadcast_operation(session_id, operation) do
    broadcast_message(session_id, {:operation, operation})
  end

  defp broadcast_error(session_id, error) do
    broadcast_message(session_id, {:error, error})
  end

  defp broadcast_message(session_id, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "sessions:#{session_id}", message)
  end

  defp put_memory_usage(state, runtime) do
    put_in(state.memory_usage, %{runtime: runtime, system: Livebook.SystemResources.memory()})
  end

  defp set_runtime_secrets(state, secrets) do
    secrets = Enum.map(secrets, &{"LB_#{&1.label}", &1.value})
    Runtime.put_system_envs(state.data.runtime, secrets)
  end

  defp notify_update(state) do
    session = self_from_state(state)
    Livebook.Sessions.update_session(session)
    broadcast_message(state.session_id, {:session_updated, session})
    state
  end

  defp maybe_save_notebook_async(state) do
    {file, default?} = notebook_autosave_file(state)

    if file && should_save_notebook?(state) do
      pid = self()
      notebook = state.data.notebook

      {:ok, pid} =
        Task.Supervisor.start_child(Livebook.TaskSupervisor, fn ->
          content = LiveMarkdown.notebook_to_livemd(notebook)
          result = FileSystem.File.write(file, content)
          send(pid, {:save_finished, self(), result, file, default?})
        end)

      %{state | save_task_pid: pid}
    else
      state
    end
  end

  defp maybe_save_notebook_sync(state) do
    {file, default?} = notebook_autosave_file(state)

    if file && should_save_notebook?(state) do
      content = LiveMarkdown.notebook_to_livemd(state.data.notebook)
      result = FileSystem.File.write(file, content)
      handle_save_finished(state, result, file, default?)
    else
      state
    end
  end

  defp should_save_notebook?(state) do
    state.data.dirty and state.save_task_pid == nil
  end

  defp notebook_autosave_file(state) do
    file = state.data.file || default_notebook_file(state)
    default? = state.data.file == nil
    {file, default?}
  end

  defp default_notebook_file(state) do
    if path = state.autosave_path || Livebook.Settings.autosave_path() do
      dir = path |> FileSystem.Utils.ensure_dir_path() |> FileSystem.File.local()
      notebook_rel_path = default_notebook_path(state)
      FileSystem.File.resolve(dir, notebook_rel_path)
    end
  end

  defp default_notebook_path(state) do
    title_str = notebook_name_to_file_name(state.data.notebook.name)

    # We want a random, but deterministic part, so we
    # use a few trailing characters from the session id,
    # which are random already
    random_str = String.slice(state.session_id, -4..-1)

    [date_str, time_str, _] =
      state.created_at
      |> DateTime.to_iso8601()
      |> String.replace(["-", ":"], "_")
      |> String.split(["T", "."])

    "#{date_str}/#{time_str}_#{title_str}_#{random_str}.livemd"
  end

  defp notebook_name_to_file_name(notebook_name) do
    notebook_name
    |> String.downcase()
    |> String.replace(~r/\/+/u, " ")
    |> FileSystem.File.sanitize_path()
    |> case do
      "" -> "untitled_notebook"
      name -> name
    end
  end

  defp handle_save_finished(state, result, file, default?) do
    state =
      if default? do
        if state.saved_default_file && state.saved_default_file != file do
          FileSystem.File.remove(state.saved_default_file)
        end

        %{state | saved_default_file: file}
      else
        state
      end

    case result do
      :ok ->
        handle_operation(state, {:mark_as_not_dirty, @client_id})

      {:error, message} ->
        broadcast_error(state.session_id, "failed to save notebook - #{message}")
        state
    end
  end

  defp extract_archive!(binary, path) do
    case :erl_tar.extract({:binary, binary}, [:compressed, {:cwd, String.to_charlist(path)}]) do
      :ok ->
        :ok

      {:error, reason} ->
        File.rm_rf!(path)
        raise "failed to extract archive to #{path}, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Subscribes the caller to runtime messages under the given topic.

  Broadcasted events are encoded using `encoder`, if successful,
  the message is sent directly to `receiver_pid`, otherwise an
  `{:encoding_error, error, message}` is sent to the caller.
  """
  @spec subscribe_to_runtime_events(
          id(),
          String.t(),
          String.t(),
          (term() -> {:ok, term()} | {:error, term()}),
          pid()
        ) :: :ok | {:error, term()}
  def subscribe_to_runtime_events(session_id, topic, subtopic, encoder, receiver_pid) do
    full_topic = runtime_messages_topic(session_id, topic, subtopic)
    Phoenix.PubSub.subscribe(Livebook.PubSub, full_topic, metadata: {encoder, receiver_pid})
  end

  @doc """
  Unsubscribes the caller from runtime messages subscribed earlier
  with `subscribe_to_runtime_events/5`.
  """
  @spec unsubscribe_from_runtime_events(id(), String.t(), String.t()) :: :ok | {:error, term()}
  def unsubscribe_from_runtime_events(session_id, topic, subtopic) do
    full_topic = runtime_messages_topic(session_id, topic, subtopic)
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, full_topic)
  end

  @doc false
  def broadcast_runtime_event(session_id, topic, subtopic, message) do
    full_topic = runtime_messages_topic(session_id, topic, subtopic)
    Phoenix.PubSub.broadcast(Livebook.PubSub, full_topic, message, __MODULE__)
  end

  defp runtime_messages_topic(session_id, topic, subtopic) do
    "sessions:#{session_id}:runtime_messages:#{topic}:#{subtopic}"
  end

  @doc false
  # Custom dispatcher for broadcasting runtime events
  def dispatch(subscribers, from, message) do
    Enum.reduce(subscribers, %{}, fn
      {pid, _}, cache when pid == from ->
        cache

      {pid, {encoder, receiver_pid}}, cache ->
        case cache do
          %{^encoder => encoded_message} ->
            send(receiver_pid, encoded_message)
            cache

          %{} ->
            case encoder.(message) do
              {:ok, encoded_message} ->
                send(receiver_pid, encoded_message)
                Map.put(cache, encoder, encoded_message)

              {:error, error} ->
                send(pid, {:encoding_error, error, message})
                cache
            end
        end

      {pid, _}, cache ->
        send(pid, message)
        cache
    end)
  end

  @doc """
  Finds evaluation locator that the given cell depends on.

  By default looks up the direct evaluation parent.

  ## Options

    * `:existing` - considers only cells that have been evaluated
      as evaluation parents. Defaults to `false`
  """
  @spec find_base_locator(Data.t(), Cell.t(), Section.t(), keyword()) :: Runtime.locator()
  def find_base_locator(data, cell, section, opts \\ []) do
    parent_filter =
      if opts[:existing] do
        fn cell ->
          info = data.cell_infos[cell.id]
          Cell.evaluable?(cell) and info.eval.validity in [:evaluated, :stale]
        end
      else
        &Cell.evaluable?/1
      end

    default = {container_ref_for_section(section), nil}

    data.notebook
    |> Notebook.parent_cells_with_section(cell.id)
    |> Enum.find_value(default, fn {cell, section} ->
      parent_filter.(cell) && {container_ref_for_section(section), cell.id}
    end)
  end

  defp container_ref_for_section(%{parent_id: nil}), do: @main_container_ref
  defp container_ref_for_section(section), do: section.id

  @doc """
  Returns a suggested filename based on the session origin or name.
  """
  @spec suggested_filename(t(), String.t() | nil) :: String.t()
  def suggested_filename(session, ext \\ Livebook.LiveMarkdown.extension())

  def suggested_filename(%{origin: {:url, url}}, ext) do
    %URI{
      host: host,
      path: path
    } = URI.parse(url)

    path =
      case path do
        nil ->
          host

        path ->
          String.split(path, ~r(\/+))
          |> List.last()
      end
      |> FileSystem.File.sanitize_path()

    if ext do
      FileSystem.File.force_path_extension(path, ext)
    else
      path
    end
  end

  def suggested_filename(%{notebook_name: name}, ext) do
    path = notebook_name_to_file_name(name)

    if is_binary(ext) do
      FileSystem.File.force_path_extension(path, ext)
    else
      path
    end
  end
end
