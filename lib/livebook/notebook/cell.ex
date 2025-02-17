defmodule Livebook.Notebook.Cell do
  # Data structure representing a single cell in a notebook.
  #
  # Cell is the smallest structural unit in a notebook, in other words
  # it is a block. Depending on the cell type, it may consist of text
  # content, outputs or a specific UI.

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type id :: Utils.id()

  @type t :: Cell.Markdown.t() | Cell.Code.t() | Cell.Smart.t()

  @type type :: :markdown | :code | :smart

  @type indexed_output :: {non_neg_integer(), Livebook.Runtime.output()}

  @setup_cell_id_prefix "setup"
  @setup_cell_id "setup"

  @doc """
  Returns an empty cell of the given type.
  """
  @spec new(type()) :: t()
  def new(type)

  def new(:markdown), do: Cell.Markdown.new()
  def new(:code), do: Cell.Code.new()
  def new(:smart), do: Cell.Smart.new()

  @doc """
  Returns an atom representing the type of the given cell.
  """
  @spec type(t()) :: type()
  def type(cell)

  def type(%Cell.Code{}), do: :code
  def type(%Cell.Markdown{}), do: :markdown
  def type(%Cell.Smart{}), do: :smart

  @doc """
  Checks if the given cell can be evaluated.
  """
  @spec evaluable?(t()) :: boolean()
  def evaluable?(cell)

  def evaluable?(%Cell.Code{}), do: true
  def evaluable?(%Cell.Smart{}), do: true
  def evaluable?(_cell), do: false

  @doc """
  Extracts all inputs from the given indexed output.
  """
  @spec find_inputs_in_output(indexed_output()) :: list(Livebook.Runtime.input_output())
  def find_inputs_in_output(output)

  def find_inputs_in_output({_idx, %{type: :input} = input}) do
    [input]
  end

  def find_inputs_in_output({_idx, %{type: :control, attrs: %{type: :form, fields: fields}}}) do
    for {_field, input} <- fields, input != nil, do: input
  end

  def find_inputs_in_output({_idx, output}) when output.type in [:frame, :tabs, :grid] do
    Enum.flat_map(output.outputs, &find_inputs_in_output/1)
  end

  def find_inputs_in_output({_idx, %{type: :frame_update, update: {_update_type, new_outputs}}}) do
    Enum.flat_map(new_outputs, &find_inputs_in_output/1)
  end

  def find_inputs_in_output(_output), do: []

  @doc """
  Extracts all asset infos from the given non-indexed output.
  """
  @spec find_assets_in_output(Livebook.Runtime.output()) :: list(asset_info :: map())
  def find_assets_in_output(output)

  def find_assets_in_output(%{type: :js} = output), do: [output.js_view.assets]

  def find_assets_in_output(output) when output.type in [:frame, :tabs, :grid] do
    Enum.flat_map(output.outputs, &find_assets_in_output/1)
  end

  def find_assets_in_output(%{type: :frame_update, update: {_update_type, new_outputs}}) do
    Enum.flat_map(new_outputs, &find_assets_in_output/1)
  end

  def find_assets_in_output(_output), do: []

  @doc """
  Checks if the given cell is any of the setup code cells.
  """
  @spec setup?(t()) :: boolean()
  def setup?(cell)

  def setup?(%Cell.Code{id: @setup_cell_id_prefix <> _}), do: true
  def setup?(_cell), do: false

  @doc """
  The fixed identifier of the main setup cell.
  """
  @spec main_setup_cell_id() :: id()
  def main_setup_cell_id(), do: @setup_cell_id

  @doc """
  The identifier of extra setup cell for the given language.
  """
  @spec extra_setup_cell_id(atom()) :: id()
  def extra_setup_cell_id(language), do: "#{@setup_cell_id_prefix}-#{language}"
end
