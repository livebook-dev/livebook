defmodule LiveBook.Remote.Manager do
  use GenServer

  alias LiveBook.Remote

  @name __MODULE__

  def start(opts) do
    GenServer.start(__MODULE__, opts, name: @name)
  end

  @impl true
  def init(owner_pid: owner_pid) do
    Process.flag(:trap_exit, true)
    Process.monitor(owner_pid)

    Remote.EvaluatorSupervisor.start_link()

    {:ok, %{}}
  end

  @impl true
  def terminate(_reason, _state) do
    Remote.unload_required_modules()

    :ok
  end

  @impl true
  def handle_info({:DOWN, _, :process, _pid, _}, state) do
    {:stop, :normal, state}
  end
end
