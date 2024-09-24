defmodule Livebook.Runtime.Evaluator.ClientTracker do
  # Keeps track of connected clients as reported to the runtime by the
  # owner.
  #
  # Sends events to processes monitoring clients presence.

  use GenServer

  alias Livebook.Runtime

  @doc """
  Starts a new client tracker.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(_opts \\ []) do
    GenServer.start_link(__MODULE__, {})
  end

  @doc """
  Registeres new connected clients.
  """
  @spec register_clients(pid(), list(Runtime.client_id())) :: :ok
  def register_clients(client_tracker, client_ids) do
    GenServer.cast(client_tracker, {:register_clients, client_ids})
  end

  @doc """
  Unregisteres connected clients.
  """
  @spec unregister_clients(pid(), list(Runtime.client_id())) :: :ok
  def unregister_clients(client_tracker, client_ids) do
    GenServer.cast(client_tracker, {:unregister_clients, client_ids})
  end

  @doc """
  Subscribes the given process to client presence events.

  Returns the list of currently connected clients.
  """
  @spec monitor_clients(pid(), pid) :: list(Runtime.client_id())
  def monitor_clients(client_tracker, pid) do
    GenServer.call(client_tracker, {:monitor_clients, pid})
  end

  @impl true
  def init({}) do
    {:ok, %{client_ids: MapSet.new(), subscribers: MapSet.new()}}
  end

  @impl true
  def handle_cast({:register_clients, client_ids}, state) do
    for client_id <- client_ids, pid <- state.subscribers do
      send(pid, {:client_join, client_id})
    end

    state = update_in(state.client_ids, &Enum.into(client_ids, &1))

    {:noreply, state}
  end

  def handle_cast({:unregister_clients, client_ids}, state) do
    for client_id <- client_ids, pid <- state.subscribers do
      send(pid, {:client_leave, client_id})
    end

    state =
      update_in(state.client_ids, fn ids ->
        Enum.reduce(client_ids, ids, &MapSet.delete(&2, &1))
      end)

    {:noreply, state}
  end

  @impl true
  def handle_call({:monitor_clients, pid}, _from, state) do
    Process.monitor(pid)
    state = update_in(state.subscribers, &MapSet.put(&1, pid))
    client_ids = MapSet.to_list(state.client_ids)
    {:reply, client_ids, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    state = update_in(state.subscribers, &MapSet.delete(&1, pid))
    {:noreply, state}
  end
end
