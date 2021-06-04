defmodule Livebook.Notebook.Cell.Input do
  @moduledoc false

  # A cell with an input field.
  #
  # It consists of an input that the user may fill
  # and then read during code evaluation.

  defstruct [:id, :metadata, :type, :name, :value]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          metadata: Cell.metadata(),
          type: type(),
          name: String.t(),
          value: String.t()
        }

  @type type :: :text

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      metadata: %{},
      type: :text,
      # TODO: better default?
      name: "Input",
      value: ""
    }
  end
end
