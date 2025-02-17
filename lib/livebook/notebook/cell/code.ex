defmodule Livebook.Notebook.Cell.Code do
  # Notebook cell with evaluable code.
  #
  # It consists of text content that the user can edit and produces
  # output once evaluated.

  defstruct [
    :id,
    :source,
    :outputs,
    :language,
    :reevaluate_automatically,
    :continue_on_error
  ]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          source: String.t() | :__pruned__,
          outputs: list(Cell.indexed_output()),
          language: Livebook.Runtime.language(),
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
      language: :elixir,
      reevaluate_automatically: false,
      continue_on_error: false
    }
  end

  @doc """
  Return the list of supported langauges for code cells.
  """
  @spec languages() :: list(%{name: String.t(), language: atom()})
  def languages() do
    [
      %{name: "Elixir", language: :elixir},
      %{name: "Erlang", language: :erlang},
      %{name: "Python", language: :python}
    ]
  end
end
