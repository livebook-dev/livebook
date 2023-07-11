defmodule Livebook.Notebook.CanvasSettings do
  @moduledoc false

  # Data structure representing a canvas of the notebook.

  defstruct [:items]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type id :: Utils.id()

  @type t :: %__MODULE__{
          items: %{id() => item()}
        }

  @type item :: %{
          x: non_neg_integer(),
          y: non_neg_integer(),
          w: non_neg_integer(),
          h: non_neg_integer(),
          outputs: list(Cell.indexed_output())
        }

  @doc """
  Returns a blank canvas.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      items: %{}
    }
  end
end
