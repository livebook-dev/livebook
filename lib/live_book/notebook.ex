defmodule LiveBook.Notebook do
  @moduledoc false

  # Data structure representing a notebook.
  #
  # A notebook is just the representation and roughly
  # maps to a file that the user can edit.
  # A notebook *session* is a living process that holds a specific
  # notebook instance and allows users to collaboratively apply
  # changes to this notebook.
  #
  # A notebook is divided into a set of isolated *sections*.

  defstruct [:name, :version, :sections, :metadata]

  alias LiveBook.Notebook.{Section, Cell}

  @type t :: %__MODULE__{
          name: String.t(),
          version: String.t(),
          sections: list(Section.t()),
          metadata: %{String.t() => term()}
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
  Finds a cell being `offset` from the given cell within the same section.
  """
  @spec fetch_cell_sibling(t(), Cell.id(), integer()) :: {:ok, Cell.t()} | :error
  def fetch_cell_sibling(notebook, cell_id, offset) do
    with {:ok, cell, section} <- fetch_cell_and_section(notebook, cell_id) do
      idx = Enum.find_index(section.cells, &(&1 == cell))
      sibling_idx = idx + offset

      if sibling_idx >= 0 and sibling_idx < length(section.cells) do
        {:ok, Enum.at(section.cells, sibling_idx)}
      else
        :error
      end
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
  Inserts `cell` at the given `index` within section identified by `section_id`.
  """
  @spec insert_cell(t(), Section.id(), integer(), Cell.t()) :: t()
  def insert_cell(notebook, section_id, index, cell) do
    sections =
      Enum.map(notebook.sections, fn section ->
        if section.id == section_id do
          %{section | cells: List.insert_at(section.cells, index, cell)}
        else
          section
        end
      end)

    %{notebook | sections: sections}
  end

  @doc """
  Deletes section with the given id.
  """
  @spec delete_section(t(), Section.id()) :: t()
  def delete_section(notebook, section_id) do
    sections = Enum.reject(notebook.sections, &(&1.id == section_id))

    %{notebook | sections: sections}
  end

  @doc """
  Deletes cell with the given id.
  """
  @spec delete_cell(t(), Cell.id()) :: t()
  def delete_cell(notebook, cell_id) do
    sections =
      Enum.map(notebook.sections, fn section ->
        %{section | cells: Enum.reject(section.cells, &(&1.id == cell_id))}
      end)

    %{notebook | sections: sections}
  end

  @doc """
  Updates cell with the given function.
  """
  @spec update_cell(t(), Cell.id(), (Cell.t() -> Cell.t())) :: t()
  def update_cell(notebook, cell_id, fun) do
    sections =
      Enum.map(notebook.sections, fn section ->
        cells =
          Enum.map(section.cells, fn cell ->
            if cell.id == cell_id, do: fun.(cell), else: cell
          end)

        %{section | cells: cells}
      end)

    %{notebook | sections: sections}
  end

  @doc """
  Updates section with the given function.
  """
  @spec update_section(t(), Section.id(), (Section.t() -> Section.t())) :: t()
  def update_section(notebook, section_id, fun) do
    sections =
      Enum.map(notebook.sections, fn section ->
        if section.id == section_id, do: fun.(section), else: section
      end)

    %{notebook | sections: sections}
  end

  @doc """
  Returns a list of Elixir cells that the given cell depends on.

  The cells are ordered starting from the most direct parent.
  """
  @spec parent_cells(t(), Cell.id()) :: list(Cell.t())
  def parent_cells(notebook, cell_id) do
    with {:ok, _, section} <- LiveBook.Notebook.fetch_cell_and_section(notebook, cell_id) do
      # A cell depends on all previous cells within the same section.
      section.cells
      |> Enum.take_while(&(&1.id != cell_id))
      |> Enum.reverse()
      |> Enum.filter(&(&1.type == :elixir))
    else
      _ -> []
    end
  end

  @doc """
  Returns a list of Elixir cells that depend on the given cell.

  The cells are ordered starting from the most direct child.
  """
  @spec child_cells(t(), Cell.id()) :: list(Cell.t())
  def child_cells(notebook, cell_id) do
    with {:ok, _, section} <- LiveBook.Notebook.fetch_cell_and_section(notebook, cell_id) do
      # A cell affects all the cells below it within the same section.
      section.cells
      |> Enum.drop_while(&(&1.id != cell_id))
      |> Enum.drop(1)
      |> Enum.filter(&(&1.type == :elixir))
    else
      _ -> []
    end
  end
end
