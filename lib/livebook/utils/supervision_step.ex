defmodule Livebook.Utils.SupervisionStep do
  use GenServer, restart: :temporary

  @doc """
  Synchronously runs the given function as part of supervision tree
  startup.
  """
  @spec start_link({atom(), function()}) :: :ignore
  def start_link({_id, fun}) do
    GenServer.start_link(__MODULE__, fun)
  end

  @doc false
  def child_spec({id, fun}) do
    child_spec = super({id, fun})
    update_in(child_spec.id, &{&1, id})
  end

  @impl true
  def init(fun) do
    fun.()
    :ignore
  end
end
