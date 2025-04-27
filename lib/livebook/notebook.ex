defmodule Livebook.Notebook do
  # Data structure representing a notebook.
  #
  # A notebook is just a document and roughly maps to a plain file
  # that the user can edit.
  #
  # A notebook **session** is a living process that holds a specific
  # notebook instance and allows users to collaboratively apply
  # changes to that notebook. See `Livebook.Session`.
  #
  # Structurally, a notebook is divided into a number of **sections**,
  # each containing a number of **cells**.

  defstruct [
    :name,
    :setup_section,
    :sections,
    :leading_comments,
    :persist_outputs,
    :autosave_interval_s,
    :default_language,
    :output_counter,
    :app_settings,
    :hub_id,
    :hub_secret_names,
    :file_entries,
    :quarantine_file_entry_names,
    :teams_enabled,
    :deployment_group_id
  ]

  alias Livebook.Notebook
  alias Livebook.Notebook.Section
  alias Livebook.Notebook.Cell
  alias Livebook.FileSystem
  alias Livebook.Utils.Graph
  import Livebook.Utils, only: [access_by_id: 1]

  @type t :: %__MODULE__{
          name: String.t(),
          setup_section: Section.t(),
          sections: list(Section.t()),
          leading_comments: list(list(line :: String.t())),
          persist_outputs: boolean(),
          autosave_interval_s: non_neg_integer() | nil,
          default_language: :elixir | :erlang | :python,
          output_counter: non_neg_integer(),
          app_settings: Notebook.AppSettings.t(),
          hub_id: String.t(),
          hub_secret_names: list(String.t()),
          file_entries: list(file_entry()),
          quarantine_file_entry_names: MapSet.t(),
          teams_enabled: boolean(),
          deployment_group_id: String.t() | nil
        }

  @typedoc """
  File entry represents a virtual file that the notebook is aware of.

  Files can be of different types:

    * `:attachment` - a hard copy of a file managed together with the
      notebook. These files are stored in files/ directory alongside
      the notebook file

    * `:file` - absolute link to a file on any of the available file
      systems

    * `:url` - absolute link to a file available online

  ## Quarantine

  File entries of type `:file` are somewhat sensitive, since they may
  point to an external file system, like S3. When importing a notebook,
  we don't want to allow access to arbitrary files, since the user may
  not realize what exact location the file entry is pointing to. Hence,
  on import we place these file entries in "quarantine" and require
  the user to explicitly allow access to them. We persist this choice
  using the notebook stamp, so if the file is saved and opened later,
  the files are automatically allowed. If the stamp is not valid, all
  files are placed back in the quarantine.
  """
  @type file_entry ::
          %{
            name: String.t(),
            type: :attachment
          }
          | %{
              name: String.t(),
              type: :file,
              file: FileSystem.File.t()
            }
          | %{
              name: String.t(),
              type: :url,
              url: String.t()
            }

  @doc """
  Returns a blank notebook.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      name: "Untitled notebook",
      setup_section: %{Section.new() | id: "setup-section", name: "Setup", cells: []},
      sections: [],
      leading_comments: [],
      persist_outputs: default_persist_outputs(),
      autosave_interval_s: default_autosave_interval_s(),
      default_language: :elixir,
      output_counter: 0,
      app_settings: Notebook.AppSettings.new(),
      hub_id: Livebook.Hubs.Personal.id(),
      hub_secret_names: [],
      file_entries: [],
      quarantine_file_entry_names: MapSet.new(),
      teams_enabled: false,
      deployment_group_id: nil
    }
    |> put_setup_cells([Cell.new(:code)])
  end

  @doc """
  Sets the given cells as the setup section cells.
  """
  @spec put_setup_cells(t(), list(Cell.Code.t())) :: t()
  def put_setup_cells(notebook, [main_setup_cell | setup_cells]) do
    put_in(notebook.setup_section.cells, [
      %{main_setup_cell | id: Cell.main_setup_cell_id()}
      | Enum.map(setup_cells, &%{&1 | id: Cell.extra_setup_cell_id(&1.language)})
    ])
  end

  @doc """
  Returns the list of languages used by the notebook.
  """
  @spec enabled_languages(t()) :: list(atom())
  def enabled_languages(notebook) do
    python_setup_cell_id = Cell.extra_setup_cell_id(:"pyproject.toml")
    python_enabled? = Enum.any?(notebook.setup_section.cells, &(&1.id == python_setup_cell_id))
    if(python_enabled?, do: [:python], else: []) ++ [:elixir, :erlang]
  end

  @doc """
  Adds extra setup cell specific to the given language.
  """
  @spec add_extra_setup_cell(t(), atom()) :: t()
  def add_extra_setup_cell(notebook, language)

  def add_extra_setup_cell(notebook, :python) do
    cell = %{
      Cell.new(:code)
      | id: Cell.extra_setup_cell_id(:"pyproject.toml"),
        language: :"pyproject.toml",
        source: """
        [project]
        name = "project"
        version = "0.0.0"
        requires-python = "==3.13.*"
        dependencies = []\
        """
    }

    update_in(notebook.setup_section.cells, &(&1 ++ [cell]))
  end

  @doc """
  Retrieves extra setup cell specific to the given language.
  """
  @spec get_extra_setup_cell(t(), atom()) :: Cell.Code.t()
  def get_extra_setup_cell(notebook, language)

  def get_extra_setup_cell(notebook, :python) do
    id = Cell.extra_setup_cell_id(:"pyproject.toml")
    Enum.find(notebook.setup_section.cells, &(&1.id == id))
  end

  @doc """
  Returns the default value of `persist_outputs`.
  """
  @spec default_persist_outputs() :: boolean()
  def default_persist_outputs(), do: false

  @doc """
  Returns the default value of `autosave_interval_s`.
  """
  @spec default_autosave_interval_s() :: non_neg_integer()
  def default_autosave_interval_s(), do: 5

  @doc """
  Sets all persistence related properties to their default values.
  """
  @spec reset_persistence_options(t()) :: t()
  def reset_persistence_options(notebook) do
    %{
      notebook
      | persist_outputs: default_persist_outputs(),
        autosave_interval_s: default_autosave_interval_s()
    }
  end

  @doc """
  Returns all notebook sections, including the implicit ones.
  """
  @spec all_sections(t()) :: list(Section.t())
  def all_sections(notebook) do
    get_in(notebook, [access_all_sections()])
  end

  @doc """
  Finds notebook section by id.
  """
  @spec fetch_section(t(), Section.id()) :: {:ok, Section.t()} | :error
  def fetch_section(notebook, section_id) do
    notebook
    |> all_sections()
    |> Enum.find_value(:error, fn section ->
      section.id == section_id && {:ok, section}
    end)
  end

  @doc """
  Finds notebook cell by `id` and the corresponding section.
  """
  @spec fetch_cell_and_section(t(), Cell.id()) :: {:ok, Cell.t(), Section.t()} | :error
  def fetch_cell_and_section(notebook, cell_id) do
    for(
      section <- all_sections(notebook),
      cell <- section.cells,
      cell.id == cell_id,
      do: {cell, section}
    )
    |> case do
      [{cell, section}] -> {:ok, cell, section}
      [] -> :error
    end
  end

  @doc """
  Finds a cell being `offset` from the given cell (with regard to all sections).
  """
  @spec fetch_cell_sibling(t(), Cell.id(), integer()) :: {:ok, Cell.t()} | :error
  def fetch_cell_sibling(notebook, cell_id, offset) do
    all_cells = for(section <- notebook.sections, cell <- section.cells, do: cell)

    with idx when idx != nil <- Enum.find_index(all_cells, &(&1.id == cell_id)),
         sibling_idx <- idx + offset,
         true <- 0 <= sibling_idx and sibling_idx < length(all_cells) do
      {:ok, Enum.at(all_cells, sibling_idx)}
    else
      _ -> :error
    end
  end

  @doc """
  Inserts `section` at the given `index`.
  """
  @spec insert_section(t(), integer(), Section.t()) :: t()
  def insert_section(notebook, index, section) do
    sections = List.insert_at(notebook.sections, index, section)
    %{notebook | sections: sections}
  end

  @doc """
  Inserts `section` below the parent section.

  Cells below the given index are moved to the newly inserted section.
  """
  @spec insert_section_into(t(), Section.id(), non_neg_integer(), Section.t()) :: t()
  def insert_section_into(notebook, section_id, index, section) do
    {sections_above, [parent_section | sections_below]} =
      Enum.split_while(notebook.sections, &(&1.id != section_id))

    {cells_above, cells_below} = Enum.split(parent_section.cells, index)

    sections =
      sections_above ++
        [%{parent_section | cells: cells_above}, %{section | cells: cells_below}] ++
        sections_below

    %{notebook | sections: sections}
  end

  @doc """
  Inserts `cell` at the given `index` within section identified by `section_id`.
  """
  @spec insert_cell(t(), Section.id(), integer(), Cell.t()) :: t()
  def insert_cell(notebook, section_id, index, cell) do
    update_in(notebook, [Access.key(:sections), access_by_id(section_id)], fn section ->
      %{section | cells: List.insert_at(section.cells, index, cell)}
    end)
  end

  @doc """
  Deletes section with the given id.

  All cells are moved to the previous section if present.
  """
  @spec delete_section(t(), Section.id()) :: t()
  def delete_section(notebook, section_id) do
    sections =
      case Enum.split_while(notebook.sections, &(&1.id != section_id)) do
        {[], [_section | sections_below]} ->
          sections_below

        {sections_above, [section | sections_below]} ->
          {prev_section, sections_above} = List.pop_at(sections_above, length(sections_above) - 1)

          sections_above ++
            [%{prev_section | cells: prev_section.cells ++ section.cells} | sections_below]
      end

    %{notebook | sections: sections}
  end

  @doc """
  Deletes cell with the given id.
  """
  @spec delete_cell(t(), Cell.id()) :: t()
  def delete_cell(notebook, cell_id) do
    {_, notebook} =
      pop_in(notebook, [
        access_all_sections(),
        Access.all(),
        Access.key(:cells),
        access_by_id(cell_id)
      ])

    notebook
  end

  @doc """
  Updates cell with the given function.
  """
  @spec update_cell(t(), Cell.id(), (Cell.t() -> Cell.t())) :: t()
  def update_cell(notebook, cell_id, fun) do
    update_in(
      notebook,
      [access_all_sections(), Access.all(), Access.key(:cells), access_by_id(cell_id)],
      fun
    )
  end

  @doc """
  Updates all cells with the given function.
  """
  @spec update_cells(t(), (Cell.t() -> Cell.t())) :: t()
  def update_cells(notebook, fun) do
    update_in(
      notebook,
      [access_all_sections(), Access.all(), Access.key(:cells), Access.all()],
      fun
    )
  end

  @doc """
  Updates cells as `update_cells/2`, but carries an accumulator.
  """
  @spec update_reduce_cells(t(), acc, (Cell.t(), acc -> {Cell.t(), acc})) :: {t(), acc}
        when acc: term()
  def update_reduce_cells(notebook, acc, fun) do
    {[setup_section | sections], acc} =
      Enum.map_reduce([notebook.setup_section | notebook.sections], acc, fn section, acc ->
        {cells, acc} = Enum.map_reduce(section.cells, acc, fun)
        {%{section | cells: cells}, acc}
      end)

    {%{notebook | setup_section: setup_section, sections: sections}, acc}
  end

  @doc """
  Updates section with the given function.
  """
  @spec update_section(t(), Section.id(), (Section.t() -> Section.t())) :: t()
  def update_section(notebook, section_id, fun) do
    update_in(notebook, [access_all_sections(), access_by_id(section_id)], fun)
  end

  defp access_all_sections() do
    fn
      :get, %__MODULE__{} = notebook, next ->
        next.([notebook.setup_section | notebook.sections])

      :get_and_update, %__MODULE__{} = notebook, next ->
        {gets, [setup_section | sections]} = next.([notebook.setup_section | notebook.sections])
        {gets, %{notebook | setup_section: setup_section, sections: sections}}

      _op, data, _next ->
        raise "access_all_sections/0 expected %Livebook.Notebook{}, got: #{inspect(data)}"
    end
  end

  @doc """
  Moves cell by the given offset.

  The cell may move to another section if the offset indicates so.
  """
  @spec move_cell(t(), Cell.id(), integer()) :: t()
  def move_cell(notebook, cell_id, offset) do
    # We firstly create a flat list of cells interspersed with `:separator`
    # at section boundaries. Then we move the given cell by the given offset.
    # Finally we split the flat list back into cell lists
    # and put them in the corresponding sections.

    separated_cells =
      notebook.sections
      |> Enum.map_intersperse(:separator, & &1.cells)
      |> List.flatten()

    idx =
      Enum.find_index(separated_cells, fn
        :separator -> false
        cell -> cell.id == cell_id
      end)

    new_idx = (idx + offset) |> clamp_index(separated_cells)

    {cell, separated_cells} = List.pop_at(separated_cells, idx)
    separated_cells = List.insert_at(separated_cells, new_idx, cell)

    cell_groups = group_cells(separated_cells)

    sections =
      notebook.sections
      |> Enum.zip(cell_groups)
      |> Enum.map(fn {section, cells} -> %{section | cells: cells} end)

    %{notebook | sections: sections}
  end

  defp group_cells(separated_cells) do
    separated_cells
    |> Enum.reverse()
    |> do_group_cells([])
  end

  defp do_group_cells([], groups), do: groups

  defp do_group_cells([:separator | separated_cells], []) do
    do_group_cells(separated_cells, [[], []])
  end

  defp do_group_cells([:separator | separated_cells], groups) do
    do_group_cells(separated_cells, [[] | groups])
  end

  defp do_group_cells([cell | separated_cells], []) do
    do_group_cells(separated_cells, [[cell]])
  end

  defp do_group_cells([cell | separated_cells], [group | groups]) do
    do_group_cells(separated_cells, [[cell | group] | groups])
  end

  defp clamp_index(index, list) do
    index |> max(0) |> min(length(list) - 1)
  end

  @doc """
  Checks if `section` can be moved by `offset`.

  Specifically, this function checks if after the move
  all child sections are still below their parent sections.
  """
  @spec can_move_section_by?(t(), Section.t(), integer()) :: boolean()
  def can_move_section_by?(notebook, section, offset)

  def can_move_section_by?(notebook, %{parent_id: nil} = section, offset) do
    notebook.sections
    |> Enum.with_index()
    |> Enum.filter(fn {that_section, _idx} -> that_section.parent_id == section.id end)
    |> Enum.map(fn {_section, idx} -> idx end)
    |> case do
      [] ->
        true

      child_indices ->
        section_idx = section_index(notebook, section.id)
        section_idx + offset < Enum.min(child_indices)
    end
  end

  def can_move_section_by?(notebook, section, offset) do
    parent_idx = section_index(notebook, section.parent_id)
    section_idx = section_index(notebook, section.id)
    parent_idx < section_idx + offset
  end

  @doc """
  Returns sections that are valid parents for the given section.
  """
  @spec valid_parents_for(t(), Section.id()) :: list(Section.t())
  def valid_parents_for(notebook, section_id) do
    notebook.sections
    |> Enum.take_while(&(&1.id != section_id))
    |> Enum.filter(&(&1.parent_id == nil))
  end

  @doc """
  Moves section by the given offset.
  """
  @spec move_section(t(), Section.id(), integer()) :: t()
  def move_section(notebook, section_id, offset) do
    # We first find the index of the given section.
    # Then we find its' new index from given offset.
    # Finally, we move the section, and return the new notebook.

    idx = section_index(notebook, section_id)
    new_idx = (idx + offset) |> clamp_index(notebook.sections)

    {section, sections} = List.pop_at(notebook.sections, idx)
    sections = List.insert_at(sections, new_idx, section)

    %{notebook | sections: sections}
  end

  @doc """
  Returns a list of `{cell, section}` pairs including all cells.
  """
  @spec cells_with_section(t()) :: list({Cell.t(), Section.t()})
  def cells_with_section(notebook) do
    for section <- all_sections(notebook),
        cell <- section.cells,
        do: {cell, section}
  end

  @doc """
  Returns a list of `{cell, section}` pairs including all evaluable
  cells in order.
  """
  @spec evaluable_cells_with_section(t()) :: list({Cell.t(), Section.t()})
  def evaluable_cells_with_section(notebook) do
    notebook
    |> cells_with_section()
    |> Enum.filter(fn {cell, _section} -> Cell.evaluable?(cell) end)
  end

  @doc """
  Returns a list of cells (each with section) that go logically
  before the given one.

  The cells are ordered starting from the most direct parent.

  A list of cell ids can be be given, in which case parent cells
  are computed for each cell and returned as a single list.
  """
  @spec parent_cells_with_section(t(), Cell.id() | list(Cell.id())) ::
          list({Cell.t(), Section.t()})
  def parent_cells_with_section(notebook, cell_ids) when is_list(cell_ids) do
    graph = cell_dependency_graph(notebook)

    parent_cell_ids =
      for cell_id <- cell_ids,
          parent_cell_id <- Graph.find_path(graph, cell_id, nil),
          parent_cell_id not in [cell_id, nil],
          into: MapSet.new(),
          do: parent_cell_id

    notebook
    |> cells_with_section()
    |> Enum.filter(fn {cell, _} -> MapSet.member?(parent_cell_ids, cell.id) end)
    |> Enum.reverse()
  end

  def parent_cells_with_section(notebook, cell_id) do
    parent_cells_with_section(notebook, [cell_id])
  end

  @doc """
  Returns a list of cells (each with section) that go logically
  after the given one, and thus may depend on it.

  The cells are ordered starting from the most direct child.
  """
  @spec child_cells_with_section(t(), Cell.id()) :: list({Cell.t(), Section.t()})
  def child_cells_with_section(notebook, cell_id) do
    graph = cell_dependency_graph(notebook)

    child_cell_ids =
      graph
      |> Graph.leaves()
      |> Enum.flat_map(&Graph.find_path(graph, &1, cell_id))
      |> MapSet.new()
      |> MapSet.delete(cell_id)

    notebook
    |> cells_with_section()
    |> Enum.filter(fn {cell, _} -> MapSet.member?(child_cell_ids, cell.id) end)
  end

  @doc """
  Computes cell dependency graph.

  Every cell has one or none parent cells, so the graph
  is represented as a map, with cell id as the key and
  its parent cell id as the value. Cells with no parent
  are also included with the value of `nil`.

  ## Options

    * `:cell_filter` - a function determining if the given
      cell should be included in the graph. If a cell is
      excluded, transitive parenthood still applies.
      By default all cells are included.

  """
  @spec cell_dependency_graph(t()) :: Graph.t(Cell.id())
  def cell_dependency_graph(notebook, opts \\ []) do
    notebook
    |> all_sections()
    |> Enum.reduce(
      {%{}, nil, %{}},
      fn section, {graph, prev_regular_section, last_id_by_section} ->
        prev_section_id =
          if section.parent_id,
            do: section.parent_id,
            else: prev_regular_section && prev_regular_section.id

        # Cell that this section directly depends on,
        # if the section it's empty it's last id of the previous section
        prev_cell_id = prev_section_id && last_id_by_section[prev_section_id]

        {graph, last_cell_id} =
          if filter = opts[:cell_filter] do
            Enum.filter(section.cells, filter)
          else
            section.cells
          end
          |> Enum.map(& &1.id)
          |> Enum.reduce({graph, prev_cell_id}, fn cell_id, {graph, prev_cell_id} ->
            {put_in(graph[cell_id], prev_cell_id), cell_id}
          end)

        last_id_by_section = put_in(last_id_by_section[section.id], last_cell_id)

        {
          graph,
          if(section.parent_id, do: prev_regular_section, else: section),
          last_id_by_section
        }
      end
    )
    |> elem(0)
  end

  @doc """
  Returns index of the given section or `nil` if not found.
  """
  @spec section_index(t(), Section.id()) :: non_neg_integer() | nil
  def section_index(notebook, section_id) do
    Enum.find_index(notebook.sections, &(&1.id == section_id))
  end

  @doc """
  Returns a list of sections branching from the given one.
  """
  @spec child_sections(t(), Section.id()) :: list(Section.t())
  def child_sections(notebook, section_id) do
    Enum.filter(notebook.sections, &(&1.parent_id == section_id))
  end

  @doc """
  Returns a forked version of the given notebook.
  """
  @spec forked(t()) :: t()
  def forked(notebook) do
    %{notebook | name: notebook.name <> " - fork"}
  end

  @doc """
  Traverses cell outputs to find asset info matching
  the given hash.
  """
  @spec find_asset_info(t(), String.t()) :: (asset_info :: map()) | nil
  def find_asset_info(notebook, hash) do
    notebook
    |> all_sections()
    |> Enum.find_value(fn section ->
      Enum.find_value(section.cells, fn
        %Cell.Smart{js_view: %{assets: %{hash: ^hash} = assets_info}} ->
          assets_info

        %{outputs: outputs} ->
          find_assets_info_in_outputs(outputs, hash)

        _cell ->
          nil
      end)
    end)
  end

  defp find_assets_info_in_outputs(outputs, hash) do
    Enum.find_value(outputs, fn
      {_idx, %{type: :js, js_view: %{assets: %{hash: ^hash} = assets_info}}} ->
        assets_info

      {_idx, output} when output.type in [:frame, :tabs, :grid] ->
        find_assets_info_in_outputs(output.outputs, hash)

      _ ->
        nil
    end)
  end

  @doc """
  Removes all outputs from the notebook.
  """
  @spec clear_outputs(t()) :: t()
  def clear_outputs(notebook) do
    update_cells(notebook, fn
      %{outputs: _outputs} = cell -> %{cell | outputs: []}
      cell -> cell
    end)
  end

  @doc """
  Adds new output to the given cell.

  Automatically merges terminal outputs and updates frames.
  """
  @spec add_cell_output(t(), Cell.id(), Livebook.Runtime.output()) :: t()
  def add_cell_output(notebook, cell_id, output) do
    {notebook, counter} = do_add_cell_output(notebook, cell_id, notebook.output_counter, output)
    %{notebook | output_counter: counter}
  end

  defp do_add_cell_output(notebook, _cell_id, counter, %{type: :frame_update} = frame_update) do
    update_reduce_cells(notebook, counter, fn
      %{outputs: _} = cell, counter ->
        {outputs, counter} = update_frames(cell.outputs, counter, frame_update)
        {%{cell | outputs: outputs}, counter}

      cell, counter ->
        {cell, counter}
    end)
  end

  defp do_add_cell_output(notebook, cell_id, counter, output) do
    {output, counter} = index_output(output, counter)

    notebook =
      update_cell(notebook, cell_id, fn cell ->
        %{cell | outputs: add_output(cell.outputs, output)}
      end)

    {notebook, counter}
  end

  defp update_frames(outputs, counter, %{ref: ref} = frame_update) do
    Enum.map_reduce(outputs, counter, fn
      {idx, %{type: :frame, outputs: outputs, ref: ^ref} = frame}, counter ->
        {update_type, new_outputs} = frame_update.update
        {new_outputs, counter} = index_outputs(new_outputs, counter)
        outputs = apply_frame_update(outputs, new_outputs, update_type)
        {{idx, %{frame | outputs: outputs}}, counter}

      {idx, output}, counter when output.type in [:frame, :tabs, :grid] ->
        {outputs, counter} = update_frames(output.outputs, counter, frame_update)
        {{idx, %{output | outputs: outputs}}, counter}

      output, counter ->
        {output, counter}
    end)
  end

  defp apply_frame_update(_outputs, new_outputs, :replace) do
    merge_chunk_outputs(new_outputs)
  end

  defp apply_frame_update(outputs, new_outputs, :append) do
    Enum.reduce(Enum.reverse(new_outputs), outputs, &add_output(&2, &1))
  end

  defp add_output(outputs, {_idx, %{type: :ignored}}), do: outputs

  # Session clients prune rendered chunks, we only add the new one
  defp add_output(
         [{idx, %{type: type, chunk: true, text: :__pruned__} = output} | tail],
         {_idx, %{type: type, chunk: true, text: text}}
       )
       when type in [:terminal_text, :plain_text, :markdown] do
    [{idx, %{output | text: text}} | tail]
  end

  # Session server keeps all outputs, so we merge consecutive chunks
  defp add_output(
         [{idx, %{type: :terminal_text, chunk: true, text: text} = output} | tail],
         {_idx, %{type: :terminal_text, chunk: true, text: cont}}
       ) do
    [{idx, %{output | text: normalize_terminal_text(text <> cont)}} | tail]
  end

  defp add_output(outputs, {idx, %{type: :terminal_text, text: text} = output}) do
    [{idx, %{output | text: normalize_terminal_text(text)}} | outputs]
  end

  defp add_output(
         [{idx, %{type: type, chunk: true, text: text} = output} | tail],
         {_idx, %{type: type, chunk: true, text: cont}}
       )
       when type in [:plain_text, :markdown] do
    [{idx, %{output | text: text <> cont}} | tail]
  end

  defp add_output(outputs, {idx, output}) when output.type in [:frame, :grid] do
    output = update_in(output.outputs, &merge_chunk_outputs/1)
    [{idx, output} | outputs]
  end

  defp add_output(outputs, output), do: [output | outputs]

  defp merge_chunk_outputs(outputs) do
    outputs
    |> Enum.reverse()
    |> Enum.reduce([], &add_output(&2, &1))
  end

  @doc """
  Normalizes terminal text chunk.

  Handles CR rewinds and caps output lines.
  """
  @spec normalize_terminal_text(String.t()) :: String.t()
  def normalize_terminal_text(text) do
    text
    |> Livebook.Utils.apply_rewind()
    |> Livebook.Utils.cap_lines(max_terminal_lines())
  end

  @doc """
  The maximum desired number of lines of terminal text.

  This is particularly relevant for standard output, which may receive
  a lot of lines.
  """
  def max_terminal_lines(), do: 1_000

  @doc """
  Recursively adds index to all outputs, including frames.
  """
  @spec index_outputs(list(Livebook.Runtime.output()), non_neg_integer()) ::
          {list(Cell.indexed_output()), non_neg_integer()}
  def index_outputs(outputs, counter) do
    Enum.map_reduce(outputs, counter, &index_output/2)
  end

  defp index_output(output, counter) when output.type in [:frame, :tabs, :grid] do
    {outputs, counter} = index_outputs(output.outputs, counter)
    {{counter, %{output | outputs: outputs}}, counter + 1}
  end

  defp index_output(%{type: :frame_update} = output, counter) do
    {update_type, new_outputs} = output.update
    {new_outputs, counter} = index_outputs(new_outputs, counter)
    {{counter, %{output | update: {update_type, new_outputs}}}, counter + 1}
  end

  defp index_output(output, counter) do
    {{counter, output}, counter + 1}
  end

  @doc """
  Finds frame outputs matching the given ref.
  """
  @spec find_frame_outputs(t(), String.t()) :: list({Cell.indexed_output(), Cell.t()})
  def find_frame_outputs(notebook, frame_ref) do
    for section <- all_sections(notebook),
        %{outputs: outputs} = cell <- section.cells,
        output <- outputs,
        frame_output <- do_find_frame_outputs(output, frame_ref),
        do: {frame_output, cell}
  end

  defp do_find_frame_outputs({_idx, %{type: :frame, ref: ref}} = output, ref) do
    [output]
  end

  defp do_find_frame_outputs({_idx, output}, ref) when output.type in [:frame, :tabs, :grid] do
    Enum.flat_map(output.outputs, &do_find_frame_outputs(&1, ref))
  end

  defp do_find_frame_outputs(_output, _ref), do: []

  @doc """
  Removes outputs that get rendered only once.
  """
  @spec prune_cell_outputs(t()) :: t()
  def prune_cell_outputs(notebook) do
    update_cells(notebook, fn
      %{outputs: _outputs} = cell -> %{cell | outputs: prune_outputs(cell.outputs, true)}
      cell -> cell
    end)
  end

  defp prune_outputs(outputs, appendable?) do
    outputs
    |> Enum.reverse()
    |> do_prune_outputs(appendable?, [])
  end

  defp do_prune_outputs([], _appendable?, acc), do: acc

  # Keep trailing outputs that can be merged with subsequent outputs
  defp do_prune_outputs([{idx, %{type: type, chunk: true} = output}], true = _appendable?, acc)
       when type in [:terminal_text, :plain_text, :markdown] do
    [{idx, %{output | text: :__pruned__}} | acc]
  end

  # Keep frame and its relevant contents
  defp do_prune_outputs([{idx, %{type: :frame} = output} | outputs], appendable?, acc) do
    output = update_in(output.outputs, &prune_outputs(&1, true))
    do_prune_outputs(outputs, appendable?, [{idx, output} | acc])
  end

  # Keep layout output and its relevant contents
  defp do_prune_outputs([{idx, %{type: :tabs} = output} | outputs], appendable?, acc) do
    case prune_outputs(output.outputs, false) do
      [] ->
        do_prune_outputs(outputs, appendable?, acc)

      pruned_tabs_outputs ->
        output = %{output | outputs: pruned_tabs_outputs}
        do_prune_outputs(outputs, appendable?, [{idx, output} | acc])
    end
  end

  defp do_prune_outputs([{idx, %{type: :grid} = output} | outputs], appendable?, acc) do
    case prune_outputs(output.outputs, false) do
      [] ->
        do_prune_outputs(outputs, appendable?, acc)

      pruned_grid_outputs ->
        output = %{output | outputs: pruned_grid_outputs}
        do_prune_outputs(outputs, appendable?, [{idx, output} | acc])
    end
  end

  # Keep outputs that get re-rendered
  defp do_prune_outputs([{idx, output} | outputs], appendable?, acc)
       when output.type in [:input, :control, :error] do
    do_prune_outputs(outputs, appendable?, [{idx, output} | acc])
  end

  # Remove everything else
  defp do_prune_outputs([_output | outputs], appendable?, acc) do
    do_prune_outputs(outputs, appendable?, acc)
  end

  @doc """
  Validates a change is a valid file entry name.
  """
  @spec validate_file_entry_name(Ecto.Changeset.t(), atom()) :: Ecto.Changeset.t()
  def validate_file_entry_name(changeset, field) do
    changeset
    |> Ecto.Changeset.validate_format(field, ~r/^[\w-.]+$/,
      message: "should contain only alphanumeric characters, dash, underscore and dot"
    )
    |> Ecto.Changeset.validate_format(field, ~r/\.\w+$/, message: "should end with an extension")
  end

  @doc """
  Copies notebook files from one directory to another.

  Note that the source directory may have more files, only the ones
  used by the notebook are copied.

  If any of the notebook files does not exist, this function returns
  an error.
  """
  @spec copy_files(t(), FileSystem.File.t(), FileSystem.File.t()) :: :ok | {:error, String.t()}
  def copy_files(notebook, source_dir, files_dir) do
    notebook.file_entries
    |> Enum.filter(&(&1.type == :attachment))
    |> Enum.reduce_while(:ok, fn file_entry, :ok ->
      source_file = FileSystem.File.resolve(source_dir, file_entry.name)
      destination_file = FileSystem.File.resolve(files_dir, file_entry.name)

      case FileSystem.File.copy(source_file, destination_file) do
        :ok ->
          {:cont, :ok}

        {:error, error} ->
          {:halt, {:error, "failed to copy notebook file #{file_entry.name}, #{error}"}}
      end
    end)
  end
end
