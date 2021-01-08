defmodule LiveBook.Notebook do
  @moduledoc """
  Data structure representing a notebook.

  A notebook it's just the representation and roughly
  maps to a file that the user can edit.
  A notebook *session* is a living process that holds a specific
  notebook instance and allows users to collaboratively apply
  changes to this notebook.

  A notebook is divided into a set of isolated *sections*.
  """

  defstruct [:name, :version, :sections, :metadata]

  alias LiveBook.Notebook.Section

  @type t :: %__MODULE__{
          name: String.t(),
          version: String.t(),
          sections: list(Section.t()),
          metadata: %{atom() => term()}
        }

  @version "1.0"

  @doc """
  Returns a blank notebook.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      name: "Untitled notebook",
      version: @version,
      sections: [],
      metadata: %{}
    }
  end
end
