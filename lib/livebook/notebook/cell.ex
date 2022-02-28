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

  @type t :: Cell.Markdown.t() | Cell.Elixir.t() | Cell.Smart.t()

  @type type :: :markdown | :elixir | :smart

  @type indexed_output :: {non_neg_integer(), Livebook.Runtime.output()}

  @doc """
  Returns an empty cell of the given type.
  """
  @spec new(type()) :: t()
  def new(type)

  def new(:markdown), do: Cell.Markdown.new()
  def new(:elixir), do: Cell.Elixir.new()
  def new(:smart), do: Cell.Smart.new()

  @doc """
  Returns an atom representing the type of the given cell.
  """
  @spec type(t()) :: type()
  def type(cell)

  def type(%Cell.Elixir{}), do: :elixir
  def type(%Cell.Markdown{}), do: :markdown
  def type(%Cell.Smart{}), do: :smart

  @doc """
  Checks if the given cell can be evaluated.
  """
  @spec evaluable?(t()) :: boolean()
  def evaluable?(cell)

  def evaluable?(%Cell.Elixir{}), do: true
  def evaluable?(%Cell.Smart{}), do: true
  def evaluable?(_cell), do: false

  @doc """
  Extracts all inputs from the given output.
  """
  @spec find_inputs_in_output(indexed_output()) :: list(input_attrs :: map())
  def find_inputs_in_output(output)

  def find_inputs_in_output({_idx, {:input, attrs}}) do
    [attrs]
  end

  def find_inputs_in_output({_idx, {:control, %{type: :form, fields: fields}}}) do
    Keyword.values(fields)
  end

  def find_inputs_in_output({_idx, {:frame, outputs, _}}) do
    Enum.flat_map(outputs, &find_inputs_in_output/1)
  end

  def find_inputs_in_output(_output), do: []
end
