defmodule LiveBook.Notebook.Cell do
  @moduledoc """
  Data structure representing a single cell in a notebook.

  A cell is the smallest unit of work in a notebook.
  It primarly consists of text content that the user can edit
  and may potentially produce some output (e.g. during code evaluation).
  """

  defstruct [:id, :type, :source, :outputs, :metadata]

  alias LiveBook.Utils

  @type cell_id :: Utils.id()
  @type cell_type :: :markdown | :elixir

  @type t :: %__MODULE__{
          id: cell_id(),
          type: cell_type(),
          source: String.t(),
          # TODO: expand on this
          outputs: list(),
          metadata: %{atom() => term()}
        }

  @doc """
  Returns an empty cell of the given type.
  """
  @spec new(cell_type()) :: t()
  def new(type) do
    %__MODULE__{
      id: Utils.random_id(),
      type: type,
      source: "",
      outputs: [],
      metadata: %{}
    }
  end
end
