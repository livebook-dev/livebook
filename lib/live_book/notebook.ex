defmodule LiveBook.Notebook do
  @moduledoc """
  Data structure representing a notebook.

  A notebook is just the representation and roughly
  maps to a file that the user can edit.
  A notebook *session* is a living process that holds a specific
  notebook instance and allows users to collaboratively apply
  changes to this notebook.

  A notebook is divided into a set of isolated *sections*.
  """

  defstruct [:name, :version, :sections, :metadata]

  alias LiveBook.Notebook.{Section, Cell}

  @type t :: %__MODULE__{
          name: String.t(),
          version: String.t(),
          sections: list(Section.t()),
          metadata: %{atom() => term()}
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
  @spec fetch_cell_and_section(t(), Cell.section_id()) :: {:ok, Cell.t(), Section.t()} | :error
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
  Returns a list of Elixir cells that the given cell depends on.

  The cells are ordered starting from the most direct parent.
  """
  @spec parent_cells(t(), Cell.id()) :: list(Cell.t())
  def parent_cells(notebook, cell_id) do
    with {:ok, _, section} <- LiveBook.Notebook.fetch_cell_and_section(notebook, cell_id) do
      # A cell depends on all previous cells within the same section.
      section.cells
      |> Enum.filter(&(&1.type == :elixir))
      |> Enum.take_while(&(&1.id != cell_id))
      |> Enum.reverse()
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
      |> Enum.filter(&(&1.type == :elixir))
      |> Enum.reverse()
      |> Enum.take_while(&(&1.id != cell_id))
    else
      _ -> []
    end
  end
end
