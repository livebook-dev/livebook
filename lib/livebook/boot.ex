defmodule Livebook.Boot do
  use GenServer, restart: :temporary

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    Livebook.Application.boot()
    :ignore
  end
end
