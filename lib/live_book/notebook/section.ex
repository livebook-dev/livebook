defmodule LiveBook.Notebook.Section do
  @moduledoc """
  Data structure representing a single section in a notebook.

  Each section contains a number of cells and is isolated
  in the sense that cells don't interfere with cells in other sections.
  """

  defstruct [:id, :metadata, :cells]

  alias LiveBook.Notebook.Cell
  alias LiveBook.Utils

  @type section_id :: Utils.id()

  @type t :: %__MODULE__{
          id: section_id(),
          metadata: metadata(),
          cells: list(Cell.t())
        }

  @type metadata :: %{
          name: String.t()
        }

  @doc """
  Returns a blank section.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      metadata: %{
        name: "Section"
      },
      cells: []
    }
  end
end
