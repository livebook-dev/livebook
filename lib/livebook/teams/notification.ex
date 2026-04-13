defmodule Livebook.Teams.Notification do
  @type t :: %__MODULE__{
          id: String.t(),
          message: String.t() | nil,
          kind: String.t(),
          type: :deprecation | :unsupported_version | atom(),
          min_version: String.t() | nil
        }

  defstruct [:id, :message, :kind, :type, :min_version]

  def version_notification?(%__MODULE__{type: type}),
    do: type in ~w[deprecation unsupported_version]a
end
