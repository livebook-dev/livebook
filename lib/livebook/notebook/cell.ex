defmodule Livebook.Notebook.Cell do
  @moduledoc false

  # Data structure representing a single cell in a notebook.
  #
  # A cell is the smallest unit of work in a notebook.
  # It primarily consists of text content that the user can edit
  # and may potentially produce some output (e.g. during code evaluation).

  defstruct [:id, :type, :source, :outputs, :metadata]

  alias Livebook.Utils

  @type id :: Utils.id()
  @type type :: :markdown | :elixir

  @typedoc """
  Arbitrary cell information persisted as part of the notebook.

  ## Recognised entries

  * `disable_formatting` - whether this particular cell should no be automatically formatted.
     Relevant for Elixir cells only.
  """
  @type metadata :: %{String.t() => term()}

  @type t :: %__MODULE__{
          id: id(),
          type: type(),
          source: String.t(),
          outputs: list(output()),
          metadata: metadata()
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
          # Vega-Lite graphic
          | {:vega_lite_static, spec :: map()}
          # Vega-Lite graphic with dynamic data
          | {:vega_lite_dynamic, widget_process :: pid()}
          # Internal output format for errors
          | {:error, message :: binary()}

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
