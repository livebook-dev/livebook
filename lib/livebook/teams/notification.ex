defmodule Livebook.Teams.Notification do
  @type t :: %__MODULE__{
          id: String.t(),
          message: String.t() | nil,
          kind: String.t(),
          min_version: String.t() | nil
        }

  defstruct [:id, :message, :kind, :min_version]
end
