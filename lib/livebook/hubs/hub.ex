defmodule Livebook.Hubs.Hub do
  @moduledoc false
  defstruct [:id, :type, :name, :provider, :color]

  @type t :: %__MODULE__{
          id: String.t(),
          type: String.t(),
          name: String.t(),
          provider: struct(),
          color: String.t()
        }
end
