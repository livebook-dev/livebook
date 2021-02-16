defmodule LiveBook.Notebook.Cell do
  @moduledoc false

  # Data structure representing a single cell in a notebook.
  #
  # A cell is the smallest unit of work in a notebook.
  # It primarly consists of text content that the user can edit
  # and may potentially produce some output (e.g. during code evaluation).

  defstruct [:id, :type, :source, :outputs, :metadata]

  alias LiveBook.Utils

  @type id :: Utils.id()
  @type type :: :markdown | :elixir

  @type t :: %__MODULE__{
          id: id(),
          type: type(),
          source: String.t(),
          outputs: list(),
          metadata: %{atom() => term()}
        }

  @doc """
  Returns an empty cell of the given type.
  """
  @spec new(type()) :: t()
  def new(type) do
    %__MODULE__{
      id: Utils.random_id(),
      type: type,
      source: "",
      outputs: [],
      metadata: %{}
    }
  end
end
