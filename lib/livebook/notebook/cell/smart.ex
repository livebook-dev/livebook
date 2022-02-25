defmodule Livebook.Notebook.Cell.Smart do
  @moduledoc false

  # A cell with Elixir code that is edited through a dedicated UI.

  defstruct [:id, :source, :outputs, :kind, :attrs, :js_view]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          source: String.t(),
          outputs: list(Cell.indexed_output()),
          kind: String.t(),
          attrs: attrs(),
          js_view: Livebook.Runtime.js_view() | nil
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
      outputs: [],
      kind: nil,
      attrs: %{},
      js_view: nil
    }
  end
end
