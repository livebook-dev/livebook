defmodule Livebook.Hubs.Metadata do
  @moduledoc false

  defstruct [:id, :name, :provider, :color]

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          provider: struct(),
          color: String.t()
        }
end
