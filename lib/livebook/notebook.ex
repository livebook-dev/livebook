defmodule Livebook.Notebook do
  @moduledoc false

  # Data structure representing a notebook.
  #
  # A notebook is just the representation and roughly
  # maps to a file that the user can edit.
  #
  # A notebook *session* is a living process that holds a specific
  # notebook instance and allows users to collaboratively apply
  # changes to this notebook.
  #
  # A notebook is divided into a number of *sections*, each
  # containing a number of *cells*.

  defstruct [:name, :version, :sections, :metadata]

  alias Livebook.Notebook.{Section, Cell}
  import Livebook.Utils, only: [access_by_id: 1]

  @type metadata :: %{String.t() => term()}

  @type t :: %__MODULE__{
          name: String.t(),
          version: String.t(),
          sections: list(Section.t()),
          metadata: metadata()
        }

  @version "1.0"

  @doc """
  Returns a blank notebook.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      name: "Untitled notebook",
      version: @version,
      sections: [],
      metadata: %{}
    }
  end

  @doc """
  Finds notebook section by id.
  """
  @spec fetch_section(t(), Section.id()) :: {:ok, Section.t()} | :error
  def fetch_section(notebook, section_id) do
    Enum.find_value(notebook.sections, :error, fn section ->
      section.id == section_id && {:ok, section}
    end)
  end

  @doc """
  Finds notebook cell by `id` and the corresponding section.
  """
  @spec fetch_cell_and_section(t(), Cell.id()) :: {:ok, Cell.t(), Section.t()} | :error
  def fetch_cell_and_section(notebook, cell_id) do
    for(
      section <- notebook.sections,
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
        Access.key(:sections),
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
      [Access.key(:sections), Access.all(), Access.key(:cells), access_by_id(cell_id)],
      fun
    )
  end

  @doc """
  Updates section with the given function.
  """
  @spec update_section(t(), Section.id(), (Section.t() -> Section.t())) :: t()
  def update_section(notebook, section_id, fun) do
    update_in(notebook, [Access.key(:sections), access_by_id(section_id)], fun)
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
  Moves section by the given offset.
  """
  @spec move_section(t(), Section.id(), integer()) :: t()
  def move_section(notebook, section_id, offset) do
    # We first find the index of the given section.
    # Then we find its' new index from given offset.
    # Finally, we move the section, and return the new notebook.

    idx =
      Enum.find_index(notebook.sections, fn
        section -> section.id == section_id
      end)

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
    for section <- notebook.sections,
        cell <- section.cells,
        do: {cell, section}
  end

  @doc """
  Returns a list of `{cell, section}` pairs including all Elixir cells in order.
  """
  @spec elixir_cells_with_section(t()) :: list({Cell.t(), Section.t()})
  def elixir_cells_with_section(notebook) do
    notebook
    |> cells_with_section()
    |> Enum.filter(fn {cell, _section} -> is_struct(cell, Cell.Elixir) end)
  end

  @doc """
  Returns a list of cells (each with section) that go logically
  before the given one.

  The cells are ordered starting from the most direct parent.
  """
  @spec parent_cells_with_section(t(), Cell.id()) :: list({Cell.t(), Section.t()})
  def parent_cells_with_section(notebook, cell_id) do
    notebook
    |> cells_with_section()
    |> Enum.take_while(fn {cell, _} -> cell.id != cell_id end)
    |> Enum.reverse()
  end

  @doc """
  Returns a list of cells (each with section) that go logically
  after the given one, and thus may depend on it.

  The cells are ordered starting from the most direct child.
  """
  @spec child_cells_with_section(t(), Cell.id()) :: list({Cell.t(), Section.t()})
  def child_cells_with_section(notebook, cell_id) do
    notebook
    |> cells_with_section()
    |> Enum.drop_while(fn {cell, _} -> cell.id != cell_id end)
    |> Enum.drop(1)
  end

  @doc """
  Finds an input cell available to the given cell and matching
  the given prompt.
  """
  @spec input_cell_for_prompt(t(), Cell.id(), String.t()) :: {:ok, Cell.Input.t()} | :error
  def input_cell_for_prompt(notebook, cell_id, prompt) do
    notebook
    |> parent_cells_with_section(cell_id)
    |> Enum.map(fn {cell, _} -> cell end)
    |> Enum.filter(fn cell ->
      is_struct(cell, Cell.Input) and String.starts_with?(prompt, cell.name)
    end)
    |> case do
      [] ->
        :error

      input_cells ->
        cell = Enum.max_by(input_cells, &String.length(&1.name))
        {:ok, cell}
    end
  end

  @doc """
  Returns a forked version of the given notebook.
  """
  @spec forked(t()) :: t()
  def forked(notebook) do
    %{notebook | name: notebook.name <> " - fork"}
  end
end
