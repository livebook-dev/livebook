defmodule Livebook.Notebook.Cell do
  @moduledoc false

  # Data structure representing a single cell in a notebook.
  #
  # A cell is the smallest unit of work in a notebook.
  # It may consist of text content, outputs, rendered content
  # and other special forms.

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type id :: Utils.id()

  @typedoc """
  Arbitrary cell information persisted as part of the notebook.

  ## Recognised entries

  * `disable_formatting` - whether this particular cell should no be automatically formatted.
     Relevant for Elixir cells only.
  """
  @type metadata :: %{String.t() => term()}

  @type t :: Cell.Elixir.t() | Cell.Markdown.t() | Cell.Input.t()

  @doc """
  Returns an empty cell of the given type.
  """
  @spec new(:markdown | :elixir | :input) :: t()
  def new(type)

  def new(:markdown), do: Cell.Markdown.new()
  def new(:elixir), do: Cell.Elixir.new()
  def new(:input), do: Cell.Input.new()
end
