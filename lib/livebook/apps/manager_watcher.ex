defmodule Livebook.Apps.ManagerWatcher do
  # Monitors the global `Livebook.Apps.Manager` and starts a new one
  # whenever needed.

  use GenServer

  @name __MODULE__

  @doc """
  Starts a new watcher process.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {}, name: @name)
  end

  @impl true
  def init({}) do
    # At this point the DeploymentSupervisor is still starting, so we
    # start the Manager in handle_continue to avoid a dead lock
    {:ok, {}, {:continue, :after_init}}
  end

  @impl true
  def handle_continue(:after_init, {}) do
    monitor_ref = maybe_start_and_monitor()
    {:noreply, %{monitor_ref: monitor_ref}}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) when ref == state.monitor_ref do
    monitor_ref = maybe_start_and_monitor()
    {:noreply, %{monitor_ref: monitor_ref}}
  end

  defp maybe_start_and_monitor() do
    pid =
      case Livebook.Apps.DeploymentSupervisor.start_manager() do
        {:ok, pid} ->
          report_manager_started(pid)
          pid

        {:error, {:already_started, pid}} ->
          pid
      end

    Process.monitor(pid)
  end

  defp report_manager_started(pid) do
    # We use this specifically for tests
    message = {:manager_started, pid}
    Phoenix.PubSub.direct_broadcast!(node(), Livebook.PubSub, "manager_watcher", message)
  end
end
