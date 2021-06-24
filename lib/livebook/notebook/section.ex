defmodule Livebook.Notebook.Section do
  @moduledoc false

  # Data structure representing a single section in a notebook.
  #
  # Each section contains a number of cells and serves as a way
  # of grouping related cells.

  defstruct [:id, :name, :cells, :metadata]

  alias Livebook.Notebook.Cell
  alias Livebook.Utils

  @type id :: Utils.id()
  @type metadata :: %{String.t() => term()}

  @type t :: %__MODULE__{
          id: id(),
          name: String.t(),
          cells: list(Cell.t()),
          metadata: metadata()
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
