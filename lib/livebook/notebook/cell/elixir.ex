defmodule Livebook.Notebook.Cell.Elixir do
  @moduledoc false

  # A cell with Elixir code.
  #
  # It consists of text content that the user can edit
  # and produces some output once evaluated.

  defstruct [:id, :source, :outputs, :disable_formatting, :reevaluate_automatically]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          source: String.t(),
          outputs: list(indexed_output()),
          disable_formatting: boolean(),
          reevaluate_automatically: boolean()
        }

  @type indexed_output :: {non_neg_integer(), Livebook.Runtime.output()}

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      source: "",
      outputs: [],
      disable_formatting: false,
      reevaluate_automatically: false
    }
  end

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
