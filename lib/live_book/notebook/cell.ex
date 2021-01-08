defmodule LiveBook.Notebook.Cell do
  @moduledoc """
  Data structure representing a single cell in a notebook.

  A cell is the smallest unit of work in a notebook.
  It primarly consists of text content that the user can edit
  and may potentially produce some output (e.g. during code execution).
  """

  defstruct [:id, :metadata, :type, :source, :outputs]

  alias LiveBook.Utils

  @type cell_id :: Utils.id()
  @type cell_type :: :markdown | :elixir

  @type t :: %__MODULE__{
          id: cell_id(),
          metadata: metadata(),
          type: cell_type(),
          source: String.t(),
          # TODO: expand on this
          outputs: list()
        }

  @type metadata :: %{}

  @doc """
  Returns an empty cell of the given type.
  """
  @spec new(cell_type()) :: t()
  def new(type) do
    %__MODULE__{
      id: Utils.random_id(),
      metadata: %{},
      type: type,
      source: "",
      outputs: []
    }
  end
end
