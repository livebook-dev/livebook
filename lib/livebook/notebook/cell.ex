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

  @type t :: Cell.Elixir.t() | Cell.Markdown.t() | Cell.Input.t()

  @type type :: :markdown | :elixir | :input

  @doc """
  Returns an empty cell of the given type.
  """
  @spec new(type()) :: t()
  def new(type)

  def new(:markdown), do: Cell.Markdown.new()
  def new(:elixir), do: Cell.Elixir.new()
  def new(:input), do: Cell.Input.new()

  @doc """
  Returns an atom representing the type of the given cell.
  """
  @spec type(t()) :: type()
  def type(cell)

  def type(%Cell.Elixir{}), do: :elixir
  def type(%Cell.Markdown{}), do: :markdown
  def type(%Cell.Input{}), do: :input
end
