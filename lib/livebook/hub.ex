defprotocol Livebook.Hub do
  @moduledoc false

  defmodule Machine do
    @moduledoc false
    defstruct [:id, :hub, :name, :color, :token]

    @type t :: %__MODULE__{
            id: String.t(),
            hub: String.t(),
            name: String.t(),
            color: String.t(),
            token: String.t()
          }
  end

  @typep hub :: Hub.Fly.t()

  @doc """
  Gets a list of machines.
  """
  @spec fetch_machines(hub()) :: {:ok, list(Machine.t())} | {:error, any()}
  def fetch_machines(hub)
end
