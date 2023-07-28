defmodule Livebook.Session.Data do
  @moduledoc false

  # Session data is a state shared across all of the clients.
  #
  # In practice this structure is a `Notebook` decorated with all the
  # ephemeral session data.
  #
  # The data is kept both in the `Session` process and in all client
  # processes. All changes go through the `Session` process first to
  # introduce linearity and then are broadcasted to the clients, so
  # that every client receives changes in the same order. Upon
  # receiving an operation, every process applies the change to the
  # locally stored `%Data{}`. This way the local `%Data{}` stays the
  # same in all of the processes, while the messages are minimal.
  #
  # The operations cover most of the session state management, in
  # particular all notebook edits and scheduling cell evaluation.
  # See `apply_operation/2` for more details.

  defstruct [
    :notebook,
    :origin,
    :file,
    :dirty,
    :persistence_warnings,
    :section_infos,
    :cell_infos,
    :input_infos,
    :bin_entries,
    :runtime,
    :smart_cell_definitions,
    :clients_map,
    :users_map,
    :secrets,
    :hub_secrets,
    :mode,
    :deployed_app_slug,
    :app_data
  ]

  alias Livebook.{Notebook, Delta, Runtime, JSInterop, FileSystem, Hubs}
  alias Livebook.Users.User
  alias Livebook.Notebook.{Cell, Section, AppSettings}
  alias Livebook.Utils.Graph
  alias Livebook.Secrets.Secret

  @type t :: %__MODULE__{
          notebook: Notebook.t(),
          origin: Notebook.ContentLoader.location() | nil,
          file: FileSystem.File.t() | nil,
          dirty: boolean(),
          section_infos: %{Section.id() => section_info()},
          cell_infos: %{Cell.id() => cell_info()},
          input_infos: %{input_id() => input_info()},
          bin_entries: list(cell_bin_entry()),
          runtime: Runtime.t(),
          smart_cell_definitions: list(Runtime.smart_cell_definition()),
          clients_map: %{client_id() => User.id()},
          users_map: %{User.id() => User.t()},
          secrets: secrets(),
          hub_secrets: list(Secret.t()),
          mode: session_mode(),
          deployed_app_slug: String.t() | nil,
          app_data: nil | app_data()
        }

  @type section_info :: %{
          evaluating_cell_id: Cell.id(),
          evaluation_queue: MapSet.t(Cell.id())
        }

  @type cell_info :: markdown_cell_info() | code_cell_info() | smart_cell_info()

  @type markdown_cell_info :: %{
          sources: %{primary: cell_source_info()}
        }

  @type code_cell_info :: %{
          sources: %{primary: cell_source_info()},
          eval: cell_eval_info()
        }

  @type smart_cell_info :: %{
          sources: %{primary: cell_source_info(), secondary: cell_source_info()},
          eval: cell_eval_info(),
          status: smart_cell_status()
        }

  @type cell_source_tag :: atom()

  @type cell_source_info :: %{
          revision: cell_revision(),
          deltas: list(Delta.t()),
          revision_by_client_id: %{client_id() => cell_revision()},
          digest: String.t() | nil
        }

  @type cell_eval_info :: %{
          validity: cell_evaluation_validity(),
          status: cell_evaluation_status(),
          errored: boolean(),
          snapshot: snapshot(),
          evaluation_digest: String.t() | nil,
          evaluation_snapshot: snapshot() | nil,
          evaluation_time_ms: integer() | nil,
          evaluation_start: DateTime.t() | nil,
          evaluation_end: DateTime.t() | nil,
          evaluation_number: non_neg_integer(),
          outputs_batch_number: non_neg_integer(),
          bound_to_inputs: %{input_id() => input_hash()},
          new_bound_to_inputs: %{input_id() => input_hash()},
          identifiers_used: list(identifier :: term()) | :unknown,
          identifiers_defined: %{(identifier :: term()) => version :: term()},
          data: t()
        }

  @type cell_bin_entry :: %{
          cell: Cell.t(),
          section_id: Section.id(),
          section_name: String.t(),
          index: non_neg_integer(),
          deleted_at: DateTime.t()
        }

  @type cell_revision :: non_neg_integer()

  @type cell_evaluation_validity :: :fresh | :evaluated | :stale | :aborted
  @type cell_evaluation_status :: :ready | :queued | :evaluating

  @type smart_cell_status :: :dead | :starting | :started | :down

  @type input_id :: String.t()

  @type input_info :: %{value: term(), hash: input_hash()}

  @type client :: {User.id(), client_id()}

  @type client_id :: Livebook.Utils.id()

  @type index :: non_neg_integer()

  # Snapshot holds information about the cell evaluation dependencies,
  # including parent cells and inputs. Whenever the snapshot changes,
  # it implies a new evaluation context, which basically means the cell
  # got stale.
  @type snapshot :: term()

  @type input_hash :: term()

  @type input_reading :: {input_id(), input_value :: term()}

  @type secrets :: %{(name :: String.t()) => Secret.t()}

  @type session_mode :: :default | :app

  @type app_status :: %{
          # Note that technically the first state is :initial, but we always
          # expect app to start evaluating right away, so distinguishing that
          # state from :executing would not bring any value
          execution: :executing | :executed | :error | :interrupted,
          lifecycle: :active | :shutting_down | :deactivated
        }

  @type app_data :: %{
          status: app_status()
        }

  # Note that all operations carry the id of whichever client
  # originated the operation. Some operations like :apply_cell_delta
  # and :report_cell_revision require the id to be a registered
  # client, as in these cases it's necessary for the operation to
  # be properly applied. For other operations the id can represent
  # an arbitrary process and is passed for informative purposes only.

  @type operation ::
          {:set_notebook_attributes, client_id(), map()}
          | {:insert_section, client_id(), index(), Section.id()}
          | {:insert_section_into, client_id(), Section.id(), index(), Section.id()}
          | {:set_section_parent, client_id(), Section.id(), parent_id :: Section.id()}
          | {:unset_section_parent, client_id(), Section.id()}
          | {:insert_cell, client_id(), Section.id(), index(), Cell.type(), Cell.id(), map()}
          | {:delete_section, client_id(), Section.id(), delete_cells :: boolean()}
          | {:delete_cell, client_id(), Cell.id()}
          | {:restore_cell, client_id(), Cell.id()}
          | {:move_cell, client_id(), Cell.id(), offset :: integer()}
          | {:move_section, client_id(), Section.id(), offset :: integer()}
          | {:queue_cells_evaluation, client_id(), list(Cell.id())}
          | {:add_cell_doctest_report, client_id(), Cell.id(), Runtime.doctest_report()}
          | {:add_cell_evaluation_output, client_id(), Cell.id(), term()}
          | {:add_cell_evaluation_response, client_id(), Cell.id(), term(), metadata :: map()}
          | {:bind_input, client_id(), code_cell_id :: Cell.id(), input_id()}
          | {:reflect_main_evaluation_failure, client_id()}
          | {:reflect_evaluation_failure, client_id(), Section.id()}
          | {:cancel_cell_evaluation, client_id(), Cell.id()}
          | {:smart_cell_started, client_id(), Cell.id(), Delta.t(), Runtime.chunks() | nil,
             Runtime.js_view(), Runtime.editor() | nil}
          | {:update_smart_cell, client_id(), Cell.id(), Cell.Smart.attrs(), Delta.t(),
             Runtime.chunks() | nil}
          | {:queue_smart_cell_reevaluation, client_id(), Cell.id()}
          | {:smart_cell_down, client_id(), Cell.id()}
          | {:recover_smart_cell, client_id(), Cell.id()}
          | {:erase_outputs, client_id()}
          | {:set_notebook_name, client_id(), String.t()}
          | {:set_section_name, client_id(), Section.id(), String.t()}
          | {:client_join, client_id(), User.t()}
          | {:client_leave, client_id()}
          | {:update_user, client_id(), User.t()}
          | {:apply_cell_delta, client_id(), Cell.id(), cell_source_tag(), Delta.t(),
             cell_revision()}
          | {:report_cell_revision, client_id(), Cell.id(), cell_source_tag(), cell_revision()}
          | {:set_cell_attributes, client_id(), Cell.id(), map()}
          | {:set_input_value, client_id(), input_id(), value :: term()}
          | {:set_runtime, client_id(), Runtime.t()}
          | {:set_smart_cell_definitions, client_id(), list(Runtime.smart_cell_definition())}
          | {:set_file, client_id(), FileSystem.File.t() | nil}
          | {:set_autosave_interval, client_id(), non_neg_integer() | nil}
          | {:notebook_saved, client_id(), persistence_warnings :: list(String.t())}
          | {:set_secret, client_id(), Secret.t()}
          | {:unset_secret, client_id(), String.t()}
          | {:set_notebook_hub, client_id(), String.t()}
          | {:sync_hub_secrets, client_id()}
          | {:add_file_entries, client_id(), list(Notebook.file_entry())}
          | {:delete_file_entry, client_id(), String.t()}
          | {:allow_file_entry, client_id(), String.t()}
          | {:set_app_settings, client_id(), AppSettings.t()}
          | {:set_deployed_app_slug, client_id(), String.t()}
          | {:app_deactivate, client_id()}
          | {:app_shutdown, client_id()}

  @type action ::
          :connect_runtime
          | {:start_evaluation, Cell.t(), Section.t()}
          | {:stop_evaluation, Section.t()}
          | {:forget_evaluation, Cell.t(), Section.t()}
          | {:start_smart_cell, Cell.t(), Section.t()}
          | {:set_smart_cell_parents, Cell.t(), Section.t(),
             parent :: {Cell.t(), Section.t()} | nil}
          | {:report_delta, client_id(), Cell.t(), cell_source_tag(), Delta.t()}
          | {:clean_up_input_values, %{input_id() => input_info()}}
          | :app_report_status
          | :app_recover
          | :app_terminate

  @doc """
  Returns a fresh notebook session state.
  """
  @spec new(keyword()) :: t()
  def new(opts \\ []) do
    opts =
      opts
      |> Keyword.validate!([:notebook, origin: nil, mode: :default])
      |> Keyword.put_new_lazy(:notebook, &Notebook.new/0)

    notebook = opts[:notebook]

    notebook =
      if opts[:mode] == :app do
        Livebook.Notebook.clear_outputs(notebook)
      else
        notebook
      end

    default_runtime =
      if opts[:mode] == :app do
        Livebook.Config.default_app_runtime()
      else
        Livebook.Config.default_runtime()
      end

    app_data =
      if opts[:mode] == :app do
        %{status: %{execution: :executing, lifecycle: :active}}
      end

    hub = Hubs.fetch_hub!(notebook.hub_id)
    hub_secrets = Hubs.get_secrets(hub)

    startup_secrets =
      for secret <- Livebook.Secrets.get_startup_secrets(),
          do: {secret.name, secret},
          into: %{}

    secrets =
      for secret <- hub_secrets,
          secret.name in notebook.hub_secret_names,
          do: {secret.name, secret},
          into: startup_secrets

    data = %__MODULE__{
      notebook: notebook,
      origin: opts[:origin],
      file: nil,
      dirty: true,
      persistence_warnings: [],
      section_infos: initial_section_infos(notebook),
      cell_infos: initial_cell_infos(notebook),
      input_infos: initial_input_infos(notebook),
      bin_entries: [],
      runtime: default_runtime,
      smart_cell_definitions: [],
      clients_map: %{},
      users_map: %{},
      secrets: secrets,
      hub_secrets: hub_secrets,
      mode: opts[:mode],
      deployed_app_slug: nil,
      app_data: app_data
    }

    data
    |> with_actions()
    |> compute_snapshots()
    |> elem(0)
  end

  defp initial_section_infos(notebook) do
    for section <- Notebook.all_sections(notebook),
        into: %{},
        do: {section.id, new_section_info()}
  end

  defp initial_cell_infos(notebook) do
    for section <- Notebook.all_sections(notebook),
        cell <- section.cells,
        into: %{},
        do: {cell.id, new_cell_info(cell, %{})}
  end

  defp initial_input_infos(notebook) do
    for section <- Notebook.all_sections(notebook),
        cell <- section.cells,
        Cell.evaluable?(cell),
        output <- cell.outputs,
        attrs <- Cell.find_inputs_in_output(output),
        into: %{},
        do: {attrs.id, input_info(attrs.default)}
  end

  @doc """
  Applies `operation` to session `data`.

  This is a pure function, responsible only for transforming the
  state, without direct side effects. This way all processes having
  the same data can individually apply the given operation and end
  up with the same updated data.

  Since this doesn't trigger any actual processing, it becomes the
  responsibility of the caller (usually the session) to ensure the
  system reflects the updated structure. For instance, when a new
  cell is marked as evaluating, the session process should take care
  of starting actual evaluation in the runtime.

  Returns `{:ok, data, actions}` if the operation is valid, where
  `data` is the result of applying said operation to the given data
  and `actions` is a list of side effects that should be performed
  for the new data to hold true.

  Returns `:error` if the operation is not valid. An `:error` is
  oftentimes expected given the collaborative nature of sessions.
  For example, if users simultaneously delete and evaluate the same
  cell, we may apply the delete operation first, in which case the
  evaluation is no longer valid, as there is no cell with the given
  id. The `:error` response simply notifies the caller that no changes
  were applied to the state and the operation should be ignored.
  """
  @spec apply_operation(t(), operation()) :: {:ok, t(), list(action())} | :error
  def apply_operation(data, operation)

  def apply_operation(data, {:set_notebook_attributes, _client_id, attrs}) do
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

  def apply_operation(data, {:insert_section, _client_id, index, id}) do
    section = %{Section.new() | id: id}

    data
    |> with_actions()
    |> insert_section(index, section)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:insert_section_into, _client_id, section_id, index, id}) do
    with {:ok, _section} <- Notebook.fetch_section(data.notebook, section_id) do
      section = %{Section.new() | id: id}

      data
      |> with_actions()
      |> insert_section_into(section_id, index, section)
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:set_section_parent, _client_id, section_id, parent_id}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, section_id),
         {:ok, parent_section} <- Notebook.fetch_section(data.notebook, parent_id),
         true <- section.parent_id != parent_id,
         [] <- Notebook.child_sections(data.notebook, section.id),
         true <- parent_section in Notebook.valid_parents_for(data.notebook, section.id) do
      data
      |> with_actions()
      |> cancel_section_evaluation(section)
      |> set_section_parent(section, parent_section)
      |> update_validity_and_evaluation()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:unset_section_parent, _client_id, section_id}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, section_id),
         true <- section.parent_id != nil do
      data
      |> with_actions()
      |> cancel_section_evaluation(section)
      |> add_action({:stop_evaluation, section})
      |> unset_section_parent(section)
      |> update_validity_and_evaluation()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:insert_cell, _client_id, section_id, index, type, id, attrs}) do
    with {:ok, _section} <- Notebook.fetch_section(data.notebook, section_id) do
      cell = %{Cell.new(type) | id: id} |> Map.merge(attrs)

      data
      |> with_actions()
      |> insert_cell(section_id, index, cell)
      |> maybe_start_smart_cells()
      |> update_validity_and_evaluation()
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:delete_section, _client_id, id, delete_cells}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, id),
         true <- section != hd(data.notebook.sections) or delete_cells,
         [] <- Notebook.child_sections(data.notebook, section.id) do
      data
      |> with_actions()
      |> delete_section(section, delete_cells)
      |> garbage_collect_input_infos()
      |> update_validity_and_evaluation()
      |> update_smart_cell_bases(data)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:delete_cell, _client_id, id}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         false <- Cell.setup?(cell) do
      data
      |> with_actions()
      |> delete_cell(cell, section)
      |> garbage_collect_input_infos()
      |> update_validity_and_evaluation()
      |> update_smart_cell_bases(data)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:restore_cell, _client_id, id}) do
    with {:ok, cell_bin_entry} <- fetch_cell_bin_entry(data, id),
         true <- data.notebook.sections != [] do
      data
      |> with_actions()
      |> restore_cell(cell_bin_entry)
      |> update_validity_and_evaluation()
      |> maybe_start_smart_cells()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:move_cell, _client_id, id, offset}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         false <- Cell.setup?(cell),
         true <- offset != 0,
         true <- can_move_cell_by?(data, cell, section, offset) do
      data
      |> with_actions()
      |> move_cell(cell, offset)
      |> update_validity_and_evaluation()
      |> update_smart_cell_bases(data)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:move_section, _client_id, id, offset}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, id),
         true <- offset != 0,
         true <- Notebook.can_move_section_by?(data.notebook, section, offset) do
      data
      |> with_actions()
      |> move_section(section, offset)
      |> update_validity_and_evaluation()
      |> update_smart_cell_bases(data)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:queue_cells_evaluation, _client_id, cell_ids}) do
    cells_with_section =
      data.notebook
      |> Notebook.evaluable_cells_with_section()
      |> Enum.filter(fn {cell, _section} ->
        info = data.cell_infos[cell.id]
        cell.id in cell_ids and info.eval.status == :ready
      end)

    if cell_ids != [] and length(cell_ids) == length(cells_with_section) do
      cell_ids = for {cell, _section} <- cells_with_section, do: cell.id

      data
      |> with_actions()
      |> queue_prerequisite_cells_evaluation(cell_ids)
      |> reduce(cells_with_section, fn data_actions, {cell, section} ->
        queue_cell_evaluation(data_actions, cell, section)
      end)
      |> maybe_connect_runtime(data)
      |> update_validity_and_evaluation()
      |> wrap_ok()
    else
      :error
    end
  end

  def apply_operation(data, {:add_cell_evaluation_output, _client_id, id, output}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, id) do
      data
      |> with_actions()
      |> add_cell_output(cell, output)
      |> garbage_collect_input_infos()
      |> mark_dirty_if_persisting_outputs()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:add_cell_evaluation_response, _client_id, id, output, metadata}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         :evaluating <- data.cell_infos[cell.id].eval.status do
      data
      |> with_actions()
      |> add_cell_output(cell, output)
      |> finish_cell_evaluation(cell, section, metadata)
      |> garbage_collect_input_infos()
      |> update_validity_and_evaluation()
      |> update_smart_cell_bases(data)
      |> mark_dirty_if_persisting_outputs()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:add_cell_doctest_report, _client_id, id, doctest_report}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, id) do
      data
      |> with_actions()
      |> add_cell_doctest_report(cell, doctest_report)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:bind_input, _client_id, cell_id, input_id}) do
    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         Cell.evaluable?(cell),
         cell_info <- data.cell_infos[cell.id],
         :evaluating <- cell_info.eval.status,
         true <- Map.has_key?(cell_info.eval.data.input_infos, input_id),
         false <- Map.has_key?(cell_info.eval.new_bound_to_inputs, input_id) do
      data
      |> with_actions()
      |> bind_input(cell, input_id)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:reflect_main_evaluation_failure, _client_id}) do
    data
    |> with_actions()
    |> clear_main_evaluation()
    |> update_smart_cell_bases(data)
    |> app_update_execution_status()
    |> wrap_ok()
  end

  def apply_operation(data, {:reflect_evaluation_failure, _client_id, section_id}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, section_id) do
      data
      |> with_actions()
      |> clear_section_evaluation(section)
      |> update_smart_cell_bases(data)
      |> app_update_execution_status()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:cancel_cell_evaluation, _client_id, id}) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, id),
         true <- data.cell_infos[cell.id].eval.status in [:evaluating, :queued] do
      data
      |> with_actions()
      |> cancel_cell_evaluation(cell, section)
      |> update_smart_cell_bases(data)
      |> app_update_execution_status()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:smart_cell_started, client_id, id, delta, chunks, js_view, editor}) do
    with {:ok, %Cell.Smart{} = cell, _section} <-
           Notebook.fetch_cell_and_section(data.notebook, id),
         :starting <- data.cell_infos[cell.id].status do
      data
      |> with_actions()
      |> smart_cell_started(cell, client_id, delta, chunks, js_view, editor)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:update_smart_cell, client_id, id, attrs, delta, chunks}) do
    with {:ok, %Cell.Smart{} = cell, _section} <-
           Notebook.fetch_cell_and_section(data.notebook, id) do
      data
      |> with_actions()
      |> update_smart_cell(cell, client_id, attrs, delta, chunks)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:queue_smart_cell_reevaluation, _client_id, id}) do
    with {:ok, %Cell.Smart{} = cell, section} <-
           Notebook.fetch_cell_and_section(data.notebook, id),
         eval_info <- data.cell_infos[cell.id].eval,
         :ready <- eval_info.status,
         true <- eval_info.validity in [:evaluated, :stale] do
      data
      |> with_actions()
      |> queue_prerequisite_cells_evaluation([cell.id])
      |> queue_cell_evaluation(cell, section)
      |> maybe_evaluate_queued()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:smart_cell_down, _client_id, id}) do
    with {:ok, %Cell.Smart{} = cell, _section} <-
           Notebook.fetch_cell_and_section(data.notebook, id) do
      data
      |> with_actions()
      |> smart_cell_down(cell)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:recover_smart_cell, _client_id, cell_id}) do
    with {:ok, %Cell.Smart{} = cell, section} <-
           Notebook.fetch_cell_and_section(data.notebook, cell_id),
         :down <- data.cell_infos[cell_id].status do
      data
      |> with_actions()
      |> recover_smart_cell(cell, section)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:erase_outputs, _client_id}) do
    data
    |> with_actions()
    |> erase_outputs()
    |> garbage_collect_input_infos()
    |> update_smart_cell_bases(data)
    |> wrap_ok()
  end

  def apply_operation(data, {:set_notebook_name, _client_id, name}) do
    data
    |> with_actions()
    |> set_notebook_name(name)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:set_section_name, _client_id, section_id, name}) do
    with {:ok, section} <- Notebook.fetch_section(data.notebook, section_id) do
      data
      |> with_actions()
      |> set_section_name(section, name)
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:client_join, client_id, user}) do
    with false <- Map.has_key?(data.clients_map, client_id) do
      data
      |> with_actions()
      |> client_join(client_id, user)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:client_leave, client_id}) do
    with true <- Map.has_key?(data.clients_map, client_id) do
      data
      |> with_actions()
      |> client_leave(client_id)
      |> app_maybe_terminate()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:update_user, _client_id, user}) do
    with true <- Map.has_key?(data.users_map, user.id) do
      data
      |> with_actions()
      |> update_user(user)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:apply_cell_delta, client_id, cell_id, tag, delta, revision}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         source_info <- data.cell_infos[cell_id].sources[tag],
         true <- 0 < revision and revision <= source_info.revision + 1,
         # We either need to know the client, so that we can transform
         # the delta, or the delta must apply to the latest revision,
         # in which case no transformation is necessary. The latter is
         # useful when we want to apply changes programatically
         true <-
           Map.has_key?(data.clients_map, client_id) or
             revision == source_info.revision + 1 do
      data
      |> with_actions()
      |> apply_delta(client_id, cell, tag, delta, revision)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:report_cell_revision, client_id, cell_id, tag, revision}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         source_info <- data.cell_infos[cell_id].sources[tag],
         true <- 0 < revision and revision <= source_info.revision,
         true <- Map.has_key?(data.clients_map, client_id) do
      data
      |> with_actions()
      |> report_revision(client_id, cell, tag, revision)
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:set_cell_attributes, _client_id, cell_id, attrs}) do
    with {:ok, cell, _} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         true <- valid_attrs_for?(cell, attrs) do
      data
      |> with_actions()
      |> set_cell_attributes(cell, attrs)
      |> update_validity_and_evaluation()
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:set_input_value, _client_id, input_id, value}) do
    with true <- Map.has_key?(data.input_infos, input_id) do
      data
      |> with_actions()
      |> set_input_value(input_id, value)
      |> update_validity_and_evaluation()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:set_runtime, _client_id, runtime}) do
    data
    |> with_actions()
    |> set_runtime(data, runtime)
    |> wrap_ok()
  end

  def apply_operation(data, {:set_smart_cell_definitions, _client_id, definitions}) do
    data
    |> with_actions()
    |> set_smart_cell_definitions(definitions)
    |> wrap_ok()
  end

  def apply_operation(data, {:set_file, _client_id, file}) do
    data
    |> with_actions()
    |> set!(file: file)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:notebook_saved, _client_id, persistence_warnings}) do
    data
    |> with_actions()
    |> set!(persistence_warnings: persistence_warnings)
    |> set_dirty(false)
    |> wrap_ok()
  end

  def apply_operation(data, {:set_secret, _client_id, secret}) do
    data
    |> with_actions()
    |> set_secret(secret)
    |> update_notebook_hub_secret_names()
    |> wrap_ok()
  end

  def apply_operation(data, {:unset_secret, _client_id, secret_name}) do
    data
    |> with_actions()
    |> unset_secret(secret_name)
    |> update_notebook_hub_secret_names()
    |> wrap_ok()
  end

  def apply_operation(data, {:set_notebook_hub, _client_id, id}) do
    with {:ok, hub} <- Hubs.fetch_hub(id) do
      data
      |> with_actions()
      |> set_notebook_hub(hub)
      |> update_notebook_hub_secret_names()
      |> set_dirty()
      |> wrap_ok()
    end
  end

  def apply_operation(data, {:sync_hub_secrets, _client_id}) do
    data
    |> with_actions()
    |> sync_hub_secrets()
    |> update_notebook_hub_secret_names()
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:add_file_entries, _client_id, file_entries}) do
    data
    |> with_actions()
    |> unquarantine_file_entries(file_entries)
    |> add_file_entries(file_entries)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:delete_file_entry, _client_id, name}) do
    with {:ok, file_entry} <- fetch_file_entry(data.notebook, name) do
      data
      |> with_actions()
      |> unquarantine_file_entries([file_entry])
      |> delete_file_entry(file_entry)
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:allow_file_entry, _client_id, name}) do
    with {:ok, file_entry} <- fetch_file_entry(data.notebook, name) do
      data
      |> with_actions()
      |> unquarantine_file_entries([file_entry])
      |> set_dirty()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:set_app_settings, _client_id, settings}) do
    data
    |> with_actions()
    |> set_app_settings(settings)
    |> set_dirty()
    |> wrap_ok()
  end

  def apply_operation(data, {:set_deployed_app_slug, _client_id, slug}) do
    data
    |> with_actions()
    |> set_deployed_app_slug(slug)
    |> wrap_ok()
  end

  def apply_operation(data, {:app_deactivate, _client_id}) do
    with :app <- data.mode do
      data
      |> with_actions()
      |> app_deactivate()
      |> wrap_ok()
    else
      _ -> :error
    end
  end

  def apply_operation(data, {:app_shutdown, _client_id}) do
    with :app <- data.mode do
      data
      |> with_actions()
      |> app_shutdown()
      |> app_maybe_terminate()
      |> wrap_ok()
    else
      _ -> :error
    end
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
      cell_infos: Map.put(data.cell_infos, cell.id, new_cell_info(cell, data.clients_map))
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
    info = data.cell_infos[cell.id]

    data_actions =
      if is_struct(cell, Cell.Smart) and info.status in [:started, :starting] do
        add_action(data_actions, {:stop_smart_cell, cell})
      else
        data_actions
      end

    data_actions =
      if Cell.evaluable?(cell) and not pristine_evaluation?(info.eval) do
        data_actions
        |> cancel_cell_evaluation(cell, section)
        |> add_action({:forget_evaluation, cell, section})
      else
        data_actions
      end

    data_actions
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

  defp pristine_evaluation?(eval_info) do
    eval_info.validity == :fresh and eval_info.status == :ready
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

  defp can_move_cell_by?(data, cell, section, offset) do
    case data.cell_infos[cell.id] do
      %{eval: %{status: :evaluating}} ->
        notebook = Notebook.move_cell(data.notebook, cell.id, offset)
        {:ok, _cell, new_section} = Notebook.fetch_cell_and_section(notebook, cell.id)
        section.id == new_section.id

      _info ->
        true
    end
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
    relevant_cell? = &Cell.evaluable?/1
    graph_before = Notebook.cell_dependency_graph(prev_notebook, cell_filter: relevant_cell?)
    graph_after = Notebook.cell_dependency_graph(data.notebook, cell_filter: relevant_cell?)

    # For each path in the dependency graph, find the upmost cell
    # which parent changed. From that point downwards all cells
    # are potentially affected, so we unqueue them.

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
      |> Notebook.evaluable_cells_with_section()
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
      update_in(section.evaluation_queue, &MapSet.put(&1, cell.id))
    end)
    |> update_cell_eval_info!(cell.id, fn eval_info ->
      update_in(eval_info.status, fn
        :ready -> :queued
        other -> other
      end)
    end)
  end

  defp unqueue_cell_evaluation(data_actions, cell, section) do
    data_actions
    |> update_section_info!(section.id, fn section ->
      update_in(section.evaluation_queue, &MapSet.delete(&1, cell.id))
    end)
    |> update_cell_eval_info!(cell.id, &%{&1 | status: :ready})
  end

  # Rewrite older output format for backward compatibility with Kino <= 0.5.2
  defp add_cell_output(
         data_actions,
         cell,
         {:js, %{ref: ref, pid: pid, assets: assets, export: export}}
       ) do
    add_cell_output(
      data_actions,
      cell,
      {:js, %{js_view: %{ref: ref, pid: pid, assets: assets}, export: export}}
    )
  end

  defp add_cell_output({data, _} = data_actions, cell, output) do
    {[indexed_output], _counter} = Notebook.index_outputs([output], 0)

    new_input_infos =
      indexed_output
      |> Cell.find_inputs_in_output()
      |> Map.new(fn attrs -> {attrs.id, input_info(attrs.default)} end)

    {data, _} =
      data_actions =
      data_actions
      |> set!(
        notebook: Notebook.add_cell_output(data.notebook, cell.id, output),
        input_infos: Map.merge(new_input_infos, data.input_infos)
      )

    if data.cell_infos[cell.id].eval.status == :evaluating do
      # When a cell renders an input, it should also be able to read
      # that input during the same evaluation, so we make it seem as
      # if it already existed prior to the evaluation

      data_actions
      |> update_cell_eval_info!(cell.id, fn eval_info ->
        update_in(eval_info.data.input_infos, &Map.merge(new_input_infos, &1))
      end)
    else
      data_actions
    end
  end

  defp finish_cell_evaluation(data_actions, cell, section, metadata) do
    data_actions =
      data_actions
      |> update_cell_eval_info!(cell.id, fn eval_info ->
        %{
          eval_info
          | status: :ready,
            errored: metadata.errored,
            interrupted: metadata.interrupted,
            evaluation_time_ms: metadata.evaluation_time_ms,
            identifiers_used: metadata.identifiers_used,
            identifiers_defined: metadata.identifiers_defined,
            bound_to_inputs: eval_info.new_bound_to_inputs,
            evaluation_end: DateTime.utc_now(),
            code_markers: metadata.code_markers
        }
      end)
      |> update_cell_evaluation_snapshot(cell, section)
      |> set_section_info!(section.id, evaluating_cell_id: nil)

    if metadata.errored and not Map.get(cell, :continue_on_error, false) do
      data_actions
      |> unqueue_child_cells_evaluation(cell)
    else
      data_actions
    end
  end

  defp update_cell_evaluation_snapshot({data, _} = data_actions, cell, section) do
    info = data.cell_infos[cell.id]

    eval_data = data.cell_infos[cell.id].eval.data
    eval_data = put_in(eval_data.cell_infos[cell.id], info)

    graph = Notebook.cell_dependency_graph(eval_data.notebook, cell_filter: &Cell.evaluable?/1)

    cell_snapshots =
      for {cell_id, %{eval: eval_info}} <- eval_data.cell_infos,
          do: {cell_id, eval_info.snapshot},
          into: %{}

    # We compute evaluation snapshot based on the notebook state prior
    # to evaluation, but using the information about the dependencies
    # obtained during evaluation (identifiers, inputs)
    evaluation_snapshot = cell_snapshot(cell, section, graph, cell_snapshots, eval_data)

    data_actions
    |> update_cell_eval_info!(
      cell.id,
      &%{&1 | evaluation_snapshot: evaluation_snapshot, data: nil}
    )
  end

  defp add_cell_doctest_report(data_actions, cell, doctest_report) do
    data_actions
    |> update_cell_eval_info!(
      cell.id,
      &put_in(&1.doctest_reports[doctest_report.line], doctest_report)
    )
  end

  defp maybe_connect_runtime({data, _} = data_actions, prev_data) do
    if not Runtime.connected?(data.runtime) and not any_cell_queued?(prev_data) and
         any_cell_queued?(data) do
      add_action(data_actions, :connect_runtime)
    else
      data_actions
    end
  end

  defp any_cell_queued?(data) do
    Enum.any?(data.section_infos, fn {_section_id, info} ->
      not Enum.empty?(info.evaluation_queue)
    end)
  end

  defp queue_prerequisite_cells_evaluation_for_queued({data, _} = data_actions) do
    {awaiting_branch_sections, awaiting_regular_sections} =
      data.notebook
      |> Notebook.all_sections()
      |> Enum.filter(&section_awaits_evaluation?(data, &1.id))
      |> Enum.split_with(& &1.parent_id)

    trailing_queued_cell_ids =
      for section <- awaiting_branch_sections ++ Enum.take(awaiting_regular_sections, -1),
          cell = last_queued_cell(data, section),
          do: cell.id

    queue_prerequisite_cells_evaluation(data_actions, trailing_queued_cell_ids)
  end

  defp maybe_evaluate_queued({data, _} = data_actions) do
    if Runtime.connected?(data.runtime) do
      main_flow_evaluating? = main_flow_evaluating?(data)

      {awaiting_branch_sections, awaiting_regular_sections} =
        data.notebook
        |> Notebook.all_sections()
        |> Enum.filter(&section_awaits_evaluation?(data, &1.id))
        |> Enum.split_with(& &1.parent_id)

      data_actions =
        reduce(data_actions, awaiting_branch_sections, fn {data, _} = data_actions, section ->
          %{id: id} = first_queued_cell(data, section)

          {:ok, parent} = Notebook.fetch_section(data.notebook, section.parent_id)

          prev_cell_section =
            data.notebook
            |> Notebook.parent_cells_with_section(id)
            |> Enum.find_value(parent, fn {cell, section} ->
              Cell.evaluable?(cell) && section
            end)

          prev_section_queued? =
            prev_cell_section != nil and
              not Enum.empty?(data.section_infos[prev_cell_section.id].evaluation_queue)

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
    else
      # Don't trigger evaluation if we don't have a runtime started yet
      data_actions
    end
  end

  defp first_queued_cell(data, section) do
    find_queued_cell(data, section.cells)
  end

  defp last_queued_cell(data, section) do
    find_queued_cell(data, Enum.reverse(section.cells))
  end

  defp find_queued_cell(data, cells) do
    Enum.find_value(cells, fn cell ->
      info = data.cell_infos[cell.id]

      case info do
        %{eval: %{status: :queued}} -> cell
        _ -> nil
      end
    end)
  end

  defp main_flow_evaluating?(data) do
    data.notebook
    |> Notebook.all_sections()
    |> Enum.any?(fn section ->
      section.parent_id == nil and section_evaluating?(data, section.id)
    end)
  end

  defp section_evaluating?(data, section_id) do
    info = data.section_infos[section_id]
    info.evaluating_cell_id != nil
  end

  defp any_section_evaluating?(data) do
    data.notebook
    |> Notebook.all_sections()
    |> Enum.any?(fn section ->
      section_evaluating?(data, section.id)
    end)
  end

  defp section_awaits_evaluation?(data, section_id) do
    info = data.section_infos[section_id]
    info.evaluating_cell_id == nil and not Enum.empty?(info.evaluation_queue)
  end

  defp evaluate_next_cell_in_section({data, _} = data_actions, section) do
    section_info = data.section_infos[section.id]

    if section_info.evaluating_cell_id == nil and not Enum.empty?(section_info.evaluation_queue) do
      cell = first_queued_cell(data, section)

      data_actions
      |> set!(notebook: Notebook.update_cell(data.notebook, cell.id, &%{&1 | outputs: []}))
      |> update_cell_info!(cell.id, fn info ->
        update_in(info.eval, fn eval_info ->
          %{
            eval_info
            | # Note: we intentionally mark the cell as evaluating up front,
              # so that another queue operation doesn't cause duplicated
              # :start_evaluation action
              status: :evaluating,
              evaluation_number: eval_info.evaluation_number + 1,
              outputs_batch_number: eval_info.outputs_batch_number + 1,
              evaluation_digest: info.sources.primary.digest,
              new_bound_to_inputs: %{},
              # Keep the notebook state before evaluation
              data: data,
              # This is a rough estimate, the exact time is measured in the
              # evaluator itself
              evaluation_start: DateTime.utc_now(),
              evaluation_end: nil,
              code_markers: [],
              doctest_reports: %{}
          }
        end)
      end)
      |> set_section_info!(section.id,
        evaluating_cell_id: cell.id,
        evaluation_queue: MapSet.delete(section_info.evaluation_queue, cell.id)
      )
      |> add_action({:start_evaluation, cell, section})
    else
      data_actions
    end
  end

  defp bind_input(data_actions, cell, input_id) do
    data_actions
    |> update_cell_eval_info!(cell.id, fn eval_info ->
      hash = eval_info.data.input_infos[input_id].hash

      %{
        eval_info
        | new_bound_to_inputs: Map.put(eval_info.new_bound_to_inputs, input_id, hash)
      }
    end)
  end

  defp clear_all_evaluation({data, _} = data_actions) do
    data_actions
    |> reduce(Notebook.all_sections(data.notebook), &clear_section_evaluation/2)
  end

  defp clear_main_evaluation({data, _} = data_actions) do
    regular_sections =
      data.notebook
      |> Notebook.all_sections()
      |> Enum.filter(&(&1.parent_id == nil))

    data_actions
    |> reduce(regular_sections, &clear_section_evaluation/2)
  end

  defp clear_section_evaluation(data_actions, section) do
    evaluable_cells = Enum.filter(section.cells, &Cell.evaluable?/1)

    data_actions
    |> set_section_info!(section.id, evaluating_cell_id: nil, evaluation_queue: MapSet.new())
    |> reduce(
      evaluable_cells,
      &update_cell_eval_info!(&1, &2.id, fn eval_info ->
        %{
          eval_info
          | validity:
              if eval_info.validity == :fresh and eval_info.status != :evaluating do
                :fresh
              else
                :aborted
              end,
            status: :ready,
            evaluation_digest: nil,
            evaluation_snapshot: nil
        }
      end)
    )
  end

  defp queue_prerequisite_cells_evaluation({data, _} = data_actions, cell_ids) do
    prerequisites_queue =
      data.notebook
      |> Notebook.parent_cells_with_section(cell_ids)
      |> Enum.filter(fn {cell, _section} ->
        info = data.cell_infos[cell.id]
        Cell.evaluable?(cell) and cell_outdated?(data, cell) and info.eval.status == :ready
      end)
      |> Enum.reverse()

    reduce(data_actions, prerequisites_queue, fn data_actions, {cell, section} ->
      queue_cell_evaluation(data_actions, cell, section)
    end)
  end

  defp cancel_cell_evaluation({data, _} = data_actions, cell, section) do
    case get_in(data.cell_infos, [cell.id, :eval, :status]) do
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
        |> unqueue_child_cells_evaluation(cell)

      _ ->
        data_actions
    end
  end

  defp unqueue_child_cells_evaluation({data, _} = data_actions, cell) do
    evaluation_children =
      data.notebook
      |> Notebook.child_cells_with_section(cell.id)
      |> Enum.filter(fn {cell, _} -> Cell.evaluable?(cell) end)

    unqueue_cells_evaluation(data_actions, evaluation_children)
  end

  defp unqueue_cells_evaluation({data, _} = data_actions, cells_with_section) do
    queued_cells_with_section =
      Enum.filter(cells_with_section, fn {cell, _} ->
        data.cell_infos[cell.id].eval.status == :queued
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

  defp smart_cell_started(
         {data, _} = data_actions,
         cell,
         client_id,
         delta,
         chunks,
         js_view,
         editor
       ) do
    cell = %{cell | chunks: chunks, js_view: js_view, editor: editor}
    source_info = data.cell_infos[cell.id].sources.primary
    {updated_cell, source_info} = apply_delta_to_cell(cell, source_info, :primary, delta)

    data_actions
    |> set!(notebook: Notebook.update_cell(data.notebook, cell.id, fn _ -> updated_cell end))
    |> update_cell_info!(cell.id, &%{&1 | status: :started})
    |> update_cell_info!(cell.id, fn info ->
      info = %{info | status: :started}
      info = put_in(info.sources.primary, source_info)
      put_in(info.sources.secondary, new_source_info(editor && editor.source, data.clients_map))
    end)
    |> add_action({:report_delta, client_id, updated_cell, :primary, delta})
  end

  defp update_smart_cell({data, _} = data_actions, cell, client_id, attrs, delta, chunks) do
    new_attrs =
      case cell.attrs do
        :__pruned__ -> :__pruned__
        _attrs -> attrs
      end

    cell = %{cell | attrs: new_attrs, chunks: chunks}
    source_info = data.cell_infos[cell.id].sources.primary
    {updated_cell, source_info} = apply_delta_to_cell(cell, source_info, :primary, delta)

    data_actions
    |> set!(notebook: Notebook.update_cell(data.notebook, cell.id, fn _ -> updated_cell end))
    |> update_cell_info!(cell.id, fn info ->
      put_in(info.sources.primary, source_info)
    end)
    |> add_action({:report_delta, client_id, updated_cell, :primary, delta})
  end

  defp smart_cell_down(data_actions, cell) do
    data_actions
    |> update_cell_info!(cell.id, &%{&1 | status: :down})
  end

  defp recover_smart_cell({data, _} = data_actions, cell, section) do
    if Runtime.connected?(data.runtime) do
      start_smart_cell(data_actions, cell, section)
    else
      data_actions
    end
  end

  defp erase_outputs({data, _} = data_actions) do
    data_actions
    |> clear_all_evaluation()
    |> set!(notebook: Notebook.clear_outputs(data.notebook))
    |> update_every_cell_info(fn
      %{eval: _} = info ->
        info = update_in(info.eval.outputs_batch_number, &(&1 + 1))
        update_in(info.eval, &%{&1 | validity: :fresh, code_markers: [], doctest_reports: %{}})

      info ->
        info
    end)
  end

  defp set_notebook_name({data, _} = data_actions, name) do
    data_actions
    |> set!(notebook: %{data.notebook | name: name})
  end

  defp set_notebook_hub({data, _} = data_actions, hub) do
    data_actions
    |> set!(
      notebook: %{
        data.notebook
        | hub_id: hub.id,
          teams_enabled: is_struct(hub, Livebook.Hubs.Team)
      },
      hub_secrets: Hubs.get_secrets(hub)
    )
  end

  defp sync_hub_secrets({data, _} = data_actions) do
    hub = Livebook.Hubs.fetch_hub!(data.notebook.hub_id)
    secrets = Livebook.Hubs.get_secrets(hub)
    set!(data_actions, hub_secrets: secrets)
  end

  defp update_notebook_hub_secret_names({data, _} = data_actions) do
    hub_secret_names =
      for {_name, secret} <- data.secrets, secret.hub_id == data.notebook.hub_id, do: secret.name

    set!(data_actions, notebook: %{data.notebook | hub_secret_names: hub_secret_names})
  end

  defp add_file_entries({data, _} = data_actions, file_entries) do
    new_names = for entry <- file_entries, do: entry.name, into: MapSet.new()

    kept_file_entries = Enum.reject(data.notebook.file_entries, &(&1.name in new_names))

    data_actions
    |> set!(notebook: %{data.notebook | file_entries: file_entries ++ kept_file_entries})
  end

  defp delete_file_entry({data, _} = data_actions, file_entry) do
    file_entries = data.notebook.file_entries -- [file_entry]

    data_actions
    |> set!(notebook: %{data.notebook | file_entries: file_entries})
  end

  defp unquarantine_file_entries({data, _} = data_actions, file_entries) do
    names = for entry <- file_entries, do: entry.name, into: MapSet.new()

    data_actions
    |> set!(
      notebook:
        Map.update!(data.notebook, :quarantine_file_entry_names, &MapSet.difference(&1, names))
    )
  end

  defp set_section_name({data, _} = data_actions, section, name) do
    data_actions
    |> set!(notebook: Notebook.update_section(data.notebook, section.id, &%{&1 | name: name}))
  end

  defp client_join({data, _} = data_actions, client_id, user) do
    data_actions
    |> set!(
      clients_map: Map.put(data.clients_map, client_id, user.id),
      users_map: Map.put(data.users_map, user.id, user)
    )
    |> update_every_cell_info(fn
      %{sources: _} = info ->
        update_in(
          info.sources,
          &Map.new(&1, fn {key, source_info} ->
            {key, put_in(source_info.revision_by_client_id[client_id], source_info.revision)}
          end)
        )

      info ->
        info
    end)
  end

  defp client_leave({data, _} = data_actions, client_id) do
    {user_id, clients_map} = Map.pop(data.clients_map, client_id)

    users_map =
      if user_id in Map.values(clients_map) do
        data.users_map
      else
        Map.delete(data.users_map, user_id)
      end

    data_actions
    |> set!(clients_map: clients_map, users_map: users_map)
    |> update_every_cell_info(fn
      %{sources: _} = info ->
        update_in(
          info.sources,
          &Map.new(&1, fn {key, source_info} ->
            {_, source_info} = pop_in(source_info.revision_by_client_id[client_id])
            {key, purge_deltas(source_info)}
          end)
        )

      info ->
        info
    end)
  end

  defp update_user({data, _} = data_actions, user) do
    set!(data_actions, users_map: Map.put(data.users_map, user.id, user))
  end

  defp apply_delta({data, _} = data_actions, client_id, cell, tag, delta, revision) do
    source_info = data.cell_infos[cell.id].sources[tag]

    deltas_ahead = Enum.take(source_info.deltas, -(source_info.revision - revision + 1))

    transformed_new_delta =
      Enum.reduce(deltas_ahead, delta, fn delta_ahead, transformed_new_delta ->
        Delta.transform(delta_ahead, transformed_new_delta, :left)
      end)

    source_info =
      source_info
      |> Map.update!(:deltas, &(&1 ++ [transformed_new_delta]))
      |> Map.update!(:revision, &(&1 + 1))

    source_info =
      if Map.has_key?(source_info.revision_by_client_id, client_id) do
        # Before receiving acknowledgement, the client receives all
        # the other deltas, so we can assume they are in sync with
        # the server and have the same revision.
        put_in(source_info.revision_by_client_id[client_id], source_info.revision)
        |> purge_deltas()
      else
        source_info
      end

    {updated_cell, source_info} =
      apply_delta_to_cell(cell, source_info, tag, transformed_new_delta)

    data_actions
    |> set!(notebook: Notebook.update_cell(data.notebook, cell.id, fn _ -> updated_cell end))
    |> update_cell_info!(cell.id, &put_in(&1.sources[tag], source_info))
    |> add_action({:report_delta, client_id, updated_cell, tag, transformed_new_delta})
  end

  # Note: the clients drop cell's source once it's no longer needed
  defp apply_delta_to_cell(%{source: :__pruned__} = cell, source_info, _tag, _delta) do
    {cell, source_info}
  end

  defp apply_delta_to_cell(cell, source_info, tag, delta) do
    cell =
      update_in(cell, source_access(cell, tag), fn
        :__pruned__ -> :__pruned__
        source -> JSInterop.apply_delta_to_string(delta, source)
      end)

    source_info =
      case get_in(cell, source_access(cell, tag)) do
        :__pruned__ -> source_info
        source -> %{source_info | digest: :erlang.md5(source)}
      end

    {cell, source_info}
  end

  defp source_access(%Cell.Smart{}, :secondary), do: [Access.key(:editor), :source]
  defp source_access(_cell, :primary), do: [Access.key(:source)]

  defp report_revision(data_actions, client_id, cell, tag, revision) do
    data_actions
    |> update_cell_info!(cell.id, fn info ->
      update_in(info.sources[tag], fn source_info ->
        put_in(source_info.revision_by_client_id[client_id], revision)
        |> purge_deltas()
      end)
    end)
  end

  defp set_cell_attributes({data, _} = data_actions, cell, attrs) do
    data_actions
    |> set!(notebook: Notebook.update_cell(data.notebook, cell.id, &Map.merge(&1, attrs)))
  end

  defp set_input_value({data, _} = data_actions, input_id, value) do
    data_actions
    |> set!(input_infos: Map.put(data.input_infos, input_id, input_info(value)))
  end

  defp set_runtime(data_actions, prev_data, runtime) do
    {data, _} = data_actions = set!(data_actions, runtime: runtime, smart_cell_definitions: [])

    if not Runtime.connected?(prev_data.runtime) and Runtime.connected?(data.runtime) do
      data_actions
      |> maybe_evaluate_queued()
    else
      data_actions
      |> clear_all_evaluation()
      |> clear_smart_cells()
    end
  end

  defp set_secret({data, _} = data_actions, secret) do
    secrets = Map.put(data.secrets, secret.name, secret)
    set!(data_actions, secrets: secrets)
  end

  defp unset_secret({data, _} = data_actions, secret_name) do
    secrets = Map.delete(data.secrets, secret_name)
    set!(data_actions, secrets: secrets)
  end

  defp set_app_settings({data, _} = data_actions, settings) do
    set!(data_actions, notebook: %{data.notebook | app_settings: settings})
  end

  defp set_deployed_app_slug(data_actions, slug) do
    set!(data_actions, deployed_app_slug: slug)
  end

  defp app_deactivate(data_actions) do
    data_actions
    |> update_app_data!(&put_in(&1.status.lifecycle, :deactivated))
    |> add_action(:app_report_status)
  end

  defp app_shutdown(data_actions) do
    data_actions
    |> update_app_data!(&put_in(&1.status.lifecycle, :shutting_down))
    |> add_action(:app_report_status)
  end

  defp app_maybe_terminate({data, _} = data_actions) do
    if data.mode == :app and data.app_data.status.lifecycle == :shutting_down and
         data.clients_map == %{} do
      add_action(data_actions, :app_terminate)
    else
      data_actions
    end
  end

  defp set_smart_cell_definitions(data_actions, smart_cell_definitions) do
    data_actions
    |> set!(smart_cell_definitions: smart_cell_definitions)
    |> maybe_start_smart_cells()
  end

  defp maybe_start_smart_cells({data, _} = data_actions) do
    if Runtime.connected?(data.runtime) do
      dead_cells = dead_smart_cells_with_section(data)

      kinds =
        for definition <- data.smart_cell_definitions,
            definition.requirement_presets == [],
            do: definition.kind

      cells_ready_to_start = Enum.filter(dead_cells, fn {cell, _} -> cell.kind in kinds end)

      reduce(data_actions, cells_ready_to_start, fn data_actions, {cell, section} ->
        start_smart_cell(data_actions, cell, section)
      end)
    else
      data_actions
    end
  end

  defp start_smart_cell(data_actions, cell, section) do
    data_actions
    |> update_cell_info!(cell.id, &%{&1 | status: :starting})
    |> add_action({:start_smart_cell, cell, section})
  end

  defp dead_smart_cells_with_section(data) do
    for section <- Notebook.all_sections(data.notebook),
        %Cell.Smart{} = cell <- section.cells,
        info = data.cell_infos[cell.id],
        info.status == :dead,
        do: {cell, section}
  end

  defp clear_smart_cells({data, _} = data_actions) do
    {notebook, data_actions} =
      Notebook.update_reduce_cells(data.notebook, data_actions, fn
        %Cell.Smart{} = cell, data_actions ->
          {
            %{cell | js_view: nil, editor: nil},
            update_cell_info!(data_actions, cell.id, &%{&1 | status: :dead})
          }

        cell, data_actions ->
          {cell, data_actions}
      end)

    set!(data_actions, notebook: notebook)
  end

  defp purge_deltas(source_info) do
    # Given client at revision X and upstream revision Y,
    # we need Y - X last deltas that the client is not aware of,
    # so that later we can use them to transform whatever
    # the client sends us as an update.
    #
    # We find the client that is the most behind and keep
    # as many deltas as we need for them.

    min_client_revision =
      source_info.revision_by_client_id
      |> Map.values()
      |> Enum.min(fn -> source_info.revision end)

    necessary_deltas = source_info.revision - min_client_revision
    deltas = Enum.take(source_info.deltas, -necessary_deltas)

    put_in(source_info.deltas, deltas)
  end

  defp fetch_cell_bin_entry(data, cell_id) do
    Enum.find_value(data.bin_entries, :error, fn entry ->
      entry.cell.id == cell_id && {:ok, entry}
    end)
  end

  defp fetch_file_entry(notebook, name) do
    Enum.find_value(notebook.file_entries, :error, fn file_entry ->
      file_entry.name == name && {:ok, file_entry}
    end)
  end

  defp add_action({data, actions}, action) do
    {data, actions ++ [action]}
  end

  defp garbage_collect_input_infos({data, _} = data_actions) do
    if any_section_evaluating?(data) do
      # Wait if evaluation is ongoing as it may render inputs
      data_actions
    else
      used_input_ids = data.notebook |> initial_input_infos() |> Map.keys()
      {input_infos, unused_input_infos} = Map.split(data.input_infos, used_input_ids)

      if unused_input_infos == %{} do
        data_actions
      else
        data_actions
        |> set!(input_infos: input_infos)
        |> add_action({:clean_up_input_values, unused_input_infos})
      end
    end
  end

  defp update_smart_cell_bases({data, _} = data_actions, prev_data) do
    alive_smart_cell_ids =
      for {%Cell.Smart{} = cell, _} <- Notebook.cells_with_section(data.notebook),
          data.cell_infos[cell.id].status in [:started, :starting],
          into: MapSet.new(),
          do: cell.id

    if Enum.empty?(alive_smart_cell_ids) do
      data_actions
    else
      new_eval_parents = cell_evaluation_parents(data)
      prev_eval_parents = cell_evaluation_parents(prev_data)

      cell_lookup =
        data.notebook
        |> Notebook.cells_with_section()
        |> Map.new(fn {cell, section} -> {cell.id, {cell, section}} end)

      for {cell_id, eval_parents} <- new_eval_parents,
          MapSet.member?(alive_smart_cell_ids, cell_id),
          Map.has_key?(prev_eval_parents, cell_id),
          prev_eval_parents[cell_id] != eval_parents,
          reduce: data_actions do
        data_actions ->
          {cell, section} = cell_lookup[cell_id]
          parents = Enum.map(eval_parents, &cell_lookup[&1])
          add_action(data_actions, {:set_smart_cell_parents, cell, section, parents})
      end
    end
  end

  defp new_section_info() do
    %{
      evaluating_cell_id: nil,
      evaluation_queue: MapSet.new()
    }
  end

  defp new_cell_info(%Cell.Markdown{} = cell, clients_map) do
    %{
      sources: %{primary: new_source_info(cell.source, clients_map)}
    }
  end

  defp new_cell_info(%Cell.Code{} = cell, clients_map) do
    %{
      sources: %{primary: new_source_info(cell.source, clients_map)},
      eval: new_eval_info()
    }
  end

  defp new_cell_info(%Cell.Smart{} = cell, clients_map) do
    %{
      sources: %{
        primary: new_source_info(cell.source, clients_map),
        secondary: new_source_info(cell.editor && cell.editor.source, clients_map)
      },
      eval: new_eval_info(),
      status: :dead
    }
  end

  defp new_source_info(source, clients_map) do
    digest = source && :erlang.md5(source)
    client_ids = Map.keys(clients_map)

    %{
      revision: 0,
      deltas: [],
      revision_by_client_id: Map.new(client_ids, &{&1, 0}),
      digest: digest
    }
  end

  defp new_eval_info() do
    %{
      validity: :fresh,
      status: :ready,
      errored: false,
      interrupted: false,
      evaluation_digest: nil,
      evaluation_time_ms: nil,
      evaluation_start: nil,
      evaluation_end: nil,
      evaluation_number: 0,
      outputs_batch_number: 0,
      bound_to_inputs: %{},
      new_bound_to_inputs: %{},
      identifiers_used: [],
      identifiers_defined: %{},
      snapshot: nil,
      evaluation_snapshot: nil,
      data: nil,
      code_markers: [],
      doctest_reports: %{},
      reevaluates_automatically: false
    }
  end

  defp set!({data, actions}, changes) do
    changes
    |> Enum.reduce(data, fn {key, value}, info ->
      Map.replace!(info, key, value)
    end)
    |> with_actions(actions)
  end

  defp update_cell_info!({data, _} = data_actions, cell_id, fun) do
    cell_infos = Map.update!(data.cell_infos, cell_id, fun)
    set!(data_actions, cell_infos: cell_infos)
  end

  defp update_cell_eval_info!(data_actions, cell_id, fun) do
    update_cell_info!(data_actions, cell_id, &update_in(&1.eval, fun))
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

  defp update_app_data!({data, _} = data_actions, fun) do
    set!(data_actions, app_data: fun.(data.app_data))
  end

  @doc """
  Builds evaluation parent sequence for every evaluable cell.

  This function should be used instead of calling `cell_evaluation_parents/2`
  multiple times.
  """
  @spec cell_evaluation_parents(t()) :: %{Cell.id() => list(Cell.id())}
  def cell_evaluation_parents(data) do
    graph = Notebook.cell_dependency_graph(data.notebook, cell_filter: &Cell.evaluable?/1)

    graph
    |> Graph.reduce_paths({nil, %{}}, fn cell_id, {parent_id, chains} ->
      if parent_id do
        parent_chain = chains[parent_id]
        parent_info = data.cell_infos[parent_id]

        chain =
          if parent_info.eval.validity in [:evaluated, :stale] do
            [parent_id | parent_chain]
          else
            parent_chain
          end

        {cell_id, put_in(chains[cell_id], chain)}
      else
        {cell_id, put_in(chains[cell_id], [])}
      end
    end)
    |> Enum.map(&elem(&1, 1))
    |> Enum.reduce(&Map.merge/2)
  end

  @doc """
  Builds evaluation parent sequence for the given cell.

  Considers only cells that have already been evaluated.
  """
  @spec cell_evaluation_parents(Data.t(), Cell.t()) :: list({Cell.t(), Section.t()})
  def cell_evaluation_parents(data, cell) do
    for {cell, section} <- Notebook.parent_cells_with_section(data.notebook, cell.id),
        info = data.cell_infos[cell.id],
        Cell.evaluable?(cell),
        info.eval.validity in [:evaluated, :stale],
        do: {cell, section}
  end

  @doc """
  Find cells bound to the given input.
  """
  @spec bound_cells_with_section(t(), input_id()) :: list({Cell.t(), Section.t()})
  def bound_cells_with_section(data, input_id) do
    data.notebook
    |> Notebook.evaluable_cells_with_section()
    |> Enum.filter(fn {cell, _} ->
      info = data.cell_infos[cell.id]
      Map.has_key?(info.eval.bound_to_inputs, input_id)
    end)
  end

  # Computes cell snapshots and updates validity based on the new
  # values, then triggers further evaluation if applicable.
  defp update_validity_and_evaluation(data_actions) do
    data_actions
    |> compute_snapshots()
    |> update_validity()
    |> app_update_execution_status()
    |> update_reevaluates_automatically()
    # After updating validity there may be new stale cells, so we check
    # if any of them should be automatically reevaluated
    |> maybe_queue_reevaluating_cells()
    |> queue_prerequisite_cells_evaluation_for_queued()
    |> maybe_evaluate_queued()
    |> app_update_execution_status()
  end

  defp compute_snapshots({data, _} = data_actions) do
    graph = Notebook.cell_dependency_graph(data.notebook, cell_filter: &Cell.evaluable?/1)

    cells_with_section = Notebook.evaluable_cells_with_section(data.notebook)

    cell_snapshots =
      Enum.reduce(cells_with_section, %{}, fn {cell, section}, cell_snapshots ->
        snapshot = cell_snapshot(cell, section, graph, cell_snapshots, data)
        put_in(cell_snapshots[cell.id], snapshot)
      end)

    reduce(data_actions, cells_with_section, fn data_actions, {cell, _} ->
      update_cell_eval_info!(data_actions, cell.id, fn eval_info ->
        snapshot = cell_snapshots[cell.id]
        %{eval_info | snapshot: snapshot}
      end)
    end)
  end

  defp cell_snapshot(cell, section, graph, cell_snapshots, data) do
    info = data.cell_infos[cell.id]

    # Note that this is an implication of the Elixir runtime, we want
    # to reevaluate as much as possible in a branch, rather than copying
    # contexts between processes, because all structural sharing is
    # lost when copying
    is_branch? = section.parent_id != nil

    {parent_ids, identifier_versions} = identifier_deps(cell.id, graph, data)

    parent_snapshots = Enum.map(parent_ids, &cell_snapshots[&1])

    bound_input_current_hashes =
      for(
        {input_id, _hash} <- info.eval.bound_to_inputs,
        current_input_info = data.input_infos[input_id],
        do: {input_id, current_input_info.hash}
      )
      |> Enum.sort()

    deps = {is_branch?, parent_snapshots, identifier_versions, bound_input_current_hashes}

    :erlang.phash2(deps)
  end

  defp input_info(value) do
    %{value: value, hash: input_hash(value)}
  end

  defp input_hash(value) do
    :erlang.phash2(value)
  end

  defp identifier_deps(cell_id, graph, data) do
    info = data.cell_infos[cell_id]

    {parent_ids, identifier_versions} =
      case info.eval.identifiers_used do
        :unknown ->
          all_identifier_deps(graph[cell_id], graph, data)

        identifiers_used ->
          gather_identifier_deps(graph[cell_id], identifiers_used, graph, data, {[], []})
      end

    {Enum.sort(parent_ids), Enum.sort(identifier_versions)}
  end

  defp all_identifier_deps(cell_id, graph, data) do
    parent_ids = graph |> Graph.find_path(cell_id, nil) |> Enum.drop(1)

    identifier_versions =
      parent_ids
      |> List.foldr(%{}, fn cell_id, acc ->
        identifiers_defined = data.cell_infos[cell_id].eval.identifiers_defined
        Map.merge(acc, identifiers_defined)
      end)
      |> Map.to_list()

    {parent_ids, identifier_versions}
  end

  defp gather_identifier_deps(nil, _identifiers_used, _graph, _data, acc), do: acc

  defp gather_identifier_deps(_cell_id, [], _graph, _data, acc), do: acc

  defp gather_identifier_deps(
         cell_id,
         identifiers_used,
         graph,
         data,
         {parent_ids, identifier_versions}
       ) do
    identifiers_defined = data.cell_infos[cell_id].eval.identifiers_defined

    identifiers_used
    |> Enum.reduce({[], []}, fn identifier, {versions, rest_identifiers} ->
      case identifiers_defined do
        %{^identifier => version} ->
          {[{identifier, version} | versions], rest_identifiers}

        _ ->
          {versions, [identifier | rest_identifiers]}
      end
    end)
    |> case do
      {[], rest_identifiers} ->
        gather_identifier_deps(
          graph[cell_id],
          rest_identifiers,
          graph,
          data,
          {parent_ids, identifier_versions}
        )

      {versions, rest_identifiers} ->
        gather_identifier_deps(
          graph[cell_id],
          rest_identifiers,
          graph,
          data,
          {[cell_id | parent_ids], versions ++ identifier_versions}
        )
    end
  end

  defp update_validity({data, _} = data_actions) do
    cells_with_section = Notebook.evaluable_cells_with_section(data.notebook)

    reduce(data_actions, cells_with_section, fn data_actions, {cell, _} ->
      update_cell_eval_info!(data_actions, cell.id, fn eval_info ->
        validity =
          case eval_info do
            %{status: :evaluating, validity: validity} -> validity
            %{evaluation_snapshot: snapshot, snapshot: snapshot} -> :evaluated
            %{evaluation_snapshot: nil, validity: :aborted} -> :aborted
            %{evaluation_snapshot: nil} -> :fresh
            _ -> :stale
          end

        %{eval_info | validity: validity}
      end)
    end)
  end

  defp update_reevaluates_automatically({data, _} = data_actions) when data.mode == :app,
    do: data_actions

  defp update_reevaluates_automatically({data, _} = data_actions) do
    eval_parents = cell_evaluation_parents(data)

    cells_with_section = Notebook.evaluable_cells_with_section(data.notebook)
    cell_lookup = for {cell, _section} <- cells_with_section, into: %{}, do: {cell.id, cell}

    reduce(data_actions, cells_with_section, fn data_actions, {cell, _section} ->
      info = data.cell_infos[cell.id]

      # Note that we disable automatic reevaluation if any evaluation
      # parent errored (unless continue-on-error is enabled)
      reevaluates_automatically =
        Map.get(cell, :reevaluate_automatically, false) and
          info.eval.validity in [:evaluated, :stale] and
          not Enum.any?(eval_parents[cell.id], fn parent_id ->
            errored? = data.cell_infos[parent_id].eval.errored
            continue_on_error? = Map.get(cell_lookup[parent_id], :continue_on_error, false)
            errored? and not continue_on_error?
          end)

      if reevaluates_automatically == info.eval.reevaluates_automatically do
        data_actions
      else
        update_cell_eval_info!(
          data_actions,
          cell.id,
          &%{&1 | reevaluates_automatically: reevaluates_automatically}
        )
      end
    end)
  end

  defp maybe_queue_reevaluating_cells({data, _} = data_actions) do
    cells_to_reevaluate =
      data.notebook
      |> Notebook.evaluable_cells_with_section()
      |> Enum.filter(fn {cell, _section} ->
        info = data.cell_infos[cell.id]
        match?(%{status: :ready, validity: :stale, reevaluates_automatically: true}, info.eval)
      end)

    cell_ids = for {cell, _section} <- cells_to_reevaluate, do: cell.id

    data_actions
    |> queue_prerequisite_cells_evaluation(cell_ids)
    |> reduce(cells_to_reevaluate, fn data_actions, {cell, section} ->
      queue_cell_evaluation(data_actions, cell, section)
    end)
  end

  defp app_update_execution_status({data, _} = data_actions)
       when data.mode != :app,
       do: data_actions

  defp app_update_execution_status({data, _} = data_actions) do
    execution_status =
      data.notebook
      |> Notebook.evaluable_cells_with_section()
      |> Enum.find_value(:executed, fn {cell, _section} ->
        case data.cell_infos[cell.id].eval do
          %{status: :evaluating} -> :executing
          %{status: :queued} -> :executing
          %{validity: :aborted} -> :error
          %{interrupted: true} -> :interrupted
          %{errored: true} -> :error
          %{validity: :fresh} -> :executing
          _ -> nil
        end
      end)

    data_actions =
      if data.app_data.status.execution == execution_status do
        data_actions
      else
        add_action(data_actions, :app_report_status)
      end

    # If everything was executed and an error happened, it means it
    # was a runtime crash and everything is aborted
    data_actions =
      if data.app_data.status.execution == :executed and execution_status == :error do
        add_action(data_actions, :app_recover)
      else
        data_actions
      end

    update_app_data!(data_actions, &put_in(&1.status.execution, execution_status))
  end

  @doc """
  Checks if the given cell is outdated.

  A cell is considered outdated if its new/fresh or its content
  has changed since the last evaluation.
  """
  @spec cell_outdated?(t(), Cell.t()) :: boolean()
  def cell_outdated?(data, cell) do
    info = data.cell_infos[cell.id]
    info.eval.validity != :evaluated or info.eval.evaluation_digest != info.sources.primary.digest
  end

  @doc """
  Returns the list of cell ids for full evaluation.

  The list includes all outdated cells, cells in `forced_cell_ids`
  and all cells with identifier dependency on these.
  """
  @spec cell_ids_for_full_evaluation(t(), list(Cell.id())) :: list(Cell.id())
  def cell_ids_for_full_evaluation(data, forced_cell_ids) do
    evaluable_cells_with_section = Notebook.evaluable_cells_with_section(data.notebook)

    evaluable_cell_ids =
      for {cell, _} <- evaluable_cells_with_section,
          cell_outdated?(data, cell) or cell.id in forced_cell_ids,
          do: cell.id,
          into: MapSet.new()

    cell_identifier_parents = cell_identifier_parents(data)

    child_ids =
      for {cell_id, cell_identifier_parents} <- cell_identifier_parents,
          Enum.any?(cell_identifier_parents, &(&1 in evaluable_cell_ids)),
          do: cell_id

    child_ids
    |> Enum.into(evaluable_cell_ids)
    |> Enum.to_list()
    |> Enum.filter(fn cell_id ->
      info = data.cell_infos[cell_id]
      info.eval.status == :ready
    end)
  end

  # Builds identifier parent list for every evaluable cell.
  #
  # This is similar to cell_evaluation_parents, but the dependency is
  # based on identifiers used/set by each cell.
  defp cell_identifier_parents(data) do
    graph = Notebook.cell_dependency_graph(data.notebook, cell_filter: &Cell.evaluable?/1)

    graph
    |> Graph.reduce_paths(
      {nil, %{}, %{}},
      fn cell_id, {parent_id, setters, identifier_parents} ->
        if parent_id do
          cell_info = data.cell_infos[cell_id]

          direct_parents =
            case cell_info.eval.identifiers_used do
              :unknown ->
                setters |> Map.values() |> Enum.uniq()

              identifiers_used ->
                for identifier <- identifiers_used,
                    parent_id = setters[identifier],
                    uniq: true,
                    do: parent_id
            end

          parents =
            for parent_id <- direct_parents,
                cell_id <- [parent_id | identifier_parents[parent_id]],
                uniq: true,
                do: cell_id

          setters =
            for {identifier, _version} <- cell_info.eval.identifiers_defined,
                do: {identifier, cell_id},
                into: setters

          {cell_id, setters, put_in(identifier_parents[cell_id], parents)}
        else
          {cell_id, setters, put_in(identifier_parents[cell_id], [])}
        end
      end
    )
    |> Enum.map(&elem(&1, 2))
    |> Enum.reduce(&Map.merge/2)
  end

  @doc """
  Returns the list of cell ids for reevaluation.

  The list includes cells that have been evaluated, but the
  reevaluation flow ends at the first fresh cell in each branch.
  """
  @spec cell_ids_for_reevaluation(t()) :: list(Cell.id())
  def cell_ids_for_reevaluation(data) do
    data.notebook
    |> Notebook.evaluable_cells_with_section()
    |> Enum.reject(fn {cell, _section} -> Cell.setup?(cell) end)
    |> Enum.reduce_while({[], nil}, fn
      {_cell, %{id: skip_section_id} = _section}, {ids, skip_section_id} ->
        {ids, skip_section_id}

      {cell, section}, {ids, _skip_section_id} ->
        info = data.cell_infos[cell.id]

        if info.eval.validity == :fresh do
          if section.parent_id do
            {:cont, {ids, section.parent_id}}
          else
            {:halt, {ids, nil}}
          end
        else
          {:cont, {[cell.id | ids], nil}}
        end
    end)
    |> elem(0)
  end

  @doc """
  Fetches an input value for the given cell.

  If the cell is evaluating, the input value at evaluation start is
  returned instead of the current value.
  """
  @spec fetch_input_value_for_cell(t(), input_id(), Cell.id()) :: {:ok, term()} | :error
  def fetch_input_value_for_cell(data, input_id, cell_id) do
    data =
      case data.cell_infos[cell_id] do
        %{eval: %{status: :evaluating, data: data}} -> data
        _ -> data
      end

    with {:ok, info} <- Map.fetch(data.input_infos, input_id), do: {:ok, info.value}
  end

  @doc """
  Returns the set of inputs which values changed since they have been
  read by any of the cells.
  """
  @spec changed_input_ids(t()) :: MapSet.t(input_id())
  def changed_input_ids(data) do
    for {_cell_id, %{eval: eval_info}} <- data.cell_infos,
        {input_id, read_hash} <- eval_info.bound_to_inputs,
        input_info = data.input_infos[input_id],
        read_hash != input_info.hash,
        into: MapSet.new(),
        do: input_id
  end

  @doc """
  Returns a list of secrets that don't belong to the given hub
  and are effectively stored in the session only.
  """
  @spec session_secrets(secrets(), String.t()) :: list(Secret.t())
  def session_secrets(secrets, hub_id) do
    for {_name, secret} <- secrets, secret.hub_id != hub_id, do: secret
  end

  @doc """
  Checks whether the given hub secret is present in secrets.
  """
  @spec secret_toggled?(Secret.t(), secrets()) :: boolean()
  def secret_toggled?(secret, secrets) do
    Map.has_key?(secrets, secret.name) and secrets[secret.name].hub_id == secret.hub_id
  end

  @doc """
  Checks whether the given hub secret is present in secrets and has
  old value.
  """
  @spec secret_outdated?(Secret.t(), secrets()) :: boolean()
  def secret_outdated?(secret, secrets) do
    secret_used_value(secret, secrets) != secret.value
  end

  @doc """
  Returns currently used hub secret value (or actual value if not used).
  """
  @spec secret_used_value(Secret.t(), secrets()) :: String.t()
  def secret_used_value(secret, secrets) do
    if secret_toggled?(secret, secrets) do
      secrets[secret.name].value
    else
      secret.value
    end
  end
end
