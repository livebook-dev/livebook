defmodule Livebook.Notebook.Cell.Code do
  @moduledoc false

  # A cell with Elixir code.
  #
  # It consists of text content that the user can edit
  # and produces some output once evaluated.

  defstruct [
    :id,
    :source,
    :outputs,
    :disable_formatting,
    :reevaluate_automatically,
    :continue_on_error
  ]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          source: String.t(),
          outputs: list(Cell.indexed_output()),
          disable_formatting: boolean(),
          reevaluate_automatically: boolean(),
          continue_on_error: boolean()
        }

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      source: "",
      outputs: [],
      disable_formatting: false,
      reevaluate_automatically: false,
      continue_on_error: false
    }
  end
end
