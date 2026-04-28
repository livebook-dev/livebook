defmodule Livebook.Teams.Notification do
  @type t :: %__MODULE__{
          id: String.t(),
          message: String.t() | nil,
          kind: String.t()
        }

  defstruct [:id, :message, :kind]
end
