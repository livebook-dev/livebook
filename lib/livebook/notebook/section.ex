defmodule Livebook.Notebook.Section do
  @moduledoc false

  # Data structure representing a single section in a notebook.
  #
  # Each section contains a number of cells and serves as a way
  # of grouping related cells.
  #
  # A section may optionally have a parent, in which case it's
  # a branching section. Such section logically follows its
  # parent section and has no impact on any further sections.

  defstruct [:id, :name, :cells, :parent_id, :metadata]

  alias Livebook.Notebook.Cell
  alias Livebook.Utils

  @type id :: Utils.id()
  @type metadata :: %{String.t() => term()}

  @type t :: %__MODULE__{
          id: id(),
          name: String.t(),
          cells: list(Cell.t()),
          parent_id: id() | nil,
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
      parent_id: nil,
      metadata: %{}
    }
  end
end
