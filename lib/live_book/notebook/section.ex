defmodule LiveBook.Notebook.Section do
  @moduledoc false

  # Data structure representing a single section in a notebook.
  #
  # Each section contains a number of cells and is isolated
  # in the sense that cells don't interfere with cells in other sections.

  defstruct [:id, :name, :cells, :metadata]

  alias LiveBook.Notebook.Cell
  alias LiveBook.Utils

  @type id :: Utils.id()

  @type t :: %__MODULE__{
          id: id(),
          name: String.t(),
          cells: list(Cell.t()),
          metadata: %{atom() => term()}
        }

  @doc """
  Returns a blank section.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      name: "Section",
      cells: [],
      metadata: %{}
    }
  end
end
