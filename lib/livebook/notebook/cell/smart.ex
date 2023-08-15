defmodule Livebook.Notebook.Cell.Smart do
  @moduledoc false

  # A cell with evaluable code that is edited through a dedicated UI.
  #
  # Smart cell is supposed to provide the user with an easy, code-free
  # way to achieve a specific task, such as plotting a chart or querying
  # a database. The user interacts with smart cell through UI, while
  # the smart cell generates plain code to be executed. The user can
  # access and take over the underlying code at any point.
  #
  # The available smart cells come from the runtime, therefore they
  # are one Livebook's extension points.

  defstruct [:id, :source, :chunks, :outputs, :kind, :attrs, :js_view, :editor]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          source: String.t() | :__pruned__,
          chunks: Livebook.Runtime.chunks() | nil,
          outputs: list(Cell.indexed_output()),
          kind: String.t() | nil,
          attrs: attrs() | :__pruned__,
          js_view: Livebook.Runtime.js_view() | nil,
          editor: Livebook.Runtime.editor() | nil
        }

  @type attrs :: map()

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      source: "",
      chunks: nil,
      outputs: [],
      kind: nil,
      attrs: %{},
      js_view: nil,
      editor: nil
    }
  end
end
