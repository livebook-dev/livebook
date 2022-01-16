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

  @type indexed_output :: {non_neg_integer(), output()}

  @typedoc """
  For more details on output types see `t:Kino.Output.t/0`.
  """
  @type output ::
          :ignored
          # IO output, adjacent such outputs are treated as a whole
          | {:stdout, binary()}
          # Standalone text block
          | {:text, binary()}
          # Markdown content
          | {:markdown, binary()}
          # A raw image in the given format
          | {:image, content :: binary(), mime_type :: binary()}
          # Vega-Lite graphic
          | {:vega_lite_static, spec :: map()}
          # Vega-Lite graphic with dynamic data
          | {:vega_lite_dynamic, widget_process :: pid()}
          # JavaScript powered output
          | {:js, info :: map()}
          # Interactive data table
          | {:table_dynamic, widget_process :: pid()}
          # Dynamic wrapper for static output
          | {:frame_dynamic, widget_process :: pid()}
          # Outputs placeholder
          | {:frame, outputs :: list(output()), info :: map()}
          # An input field
          | {:input, attrs :: map()}
          # A control element
          | {:control, attrs :: map()}
          # Internal output format for errors
          | {:error, message :: binary(), type :: :other | :runtime_restart_required}

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
