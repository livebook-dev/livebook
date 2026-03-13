defmodule Livebook.Notebook.Cell.Markdown do
  # Notebook cell with Markdown text content.
  #
  # It consists of text content that the user can edit and which is
  # rendered on the page.

  defstruct [:id, :source, :output_size]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          source: String.t() | :__pruned__,
          output_size: Cell.output_size()
        }

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      source: "",
      output_size: :default
    }
  end
end
