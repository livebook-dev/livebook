defmodule Livebook.HubProvider.Hub do
  @moduledoc false
  defstruct [:id, :type, :name, :label, :color, :token]

  @type t :: %__MODULE__{
          id: String.t(),
          type: String.t(),
          name: String.t(),
          label: String.t(),
          color: String.t(),
          token: String.t()
        }
end
