defmodule Livebook.Notebook.Cell.Smart do
  @moduledoc false

  # A cell with Elixir code that is edited through a dedicated UI.

  defstruct [:id, :source, :chunks, :outputs, :output_location, :kind, :attrs, :js_view, :editor]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          source: String.t() | :__pruned__,
          chunks: Livebook.Runtime.chunks() | nil,
          outputs: list(Cell.indexed_output()),
          output_location: Cell.canvas_sprite() | nil,
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
      output_location: nil,
      kind: nil,
      attrs: %{},
      js_view: nil,
      editor: nil
    }
  end
end
