defmodule Livebook.Notebook.Cell.Elixir do
  @moduledoc false

  # A cell with Elixir code.
  #
  # It consists of text content that the user can edit
  # and produces some output once evaluated.

  defstruct [:id, :metadata, :source, :outputs]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          metadata: Cell.metadata(),
          source: String.t(),
          outputs: list(output())
        }

  @typedoc """
  For more details on output types see `t:Kino.Output.t/0`.
  """
  @type output ::
          :ignored
          # Regular text, adjacent such outputs can be treated as a whole
          | binary()
          # Standalone text block
          | {:text, binary()}
          # A raw image in the given format.
          | {:image, content :: binary(), mime_type :: binary()}
          # Vega-Lite graphic
          | {:vega_lite_static, spec :: map()}
          # Vega-Lite graphic with dynamic data
          | {:vega_lite_dynamic, widget_process :: pid()}
          # Interactive data table
          | {:table_dynamic, widget_process :: pid()}
          # Internal output format for errors
          | {:error, message :: binary()}

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      metadata: %{},
      source: "",
      outputs: []
    }
  end
end
