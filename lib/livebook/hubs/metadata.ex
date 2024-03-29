defmodule Livebook.Hubs.Metadata do
  defstruct [:id, :name, :provider, :emoji, connected?: false]

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          provider: struct(),
          emoji: String.t(),
          connected?: boolean()
        }
end
