defmodule Livebook.Notebook.Cell.Markdown do
  @moduledoc false

  # A cell with Markdown text content.
  #
  # It consists of Markdown content that the user can edit
  # and which is then rendered on the page.

  defstruct [:id, :source]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          source: String.t()
        }

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      source: ""
    }
  end
end
