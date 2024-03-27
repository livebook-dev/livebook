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
  @spec register_clients(pid(), list({Runtime.client_id(), Runtime.user_info()})) :: :ok
  def register_clients(client_tracker, clients) do
    GenServer.cast(client_tracker, {:register_clients, clients})
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
  """
  @spec monitor_clients(pid(), pid) :: :ok
  def monitor_clients(client_tracker, pid) do
    GenServer.cast(client_tracker, {:monitor_clients, pid})
  end

  @impl true
  def init({}) do
    {:ok, %{clients: %{}, subscribers: MapSet.new()}}
  end

  @impl true
  def handle_cast({:register_clients, clients}, state) do
    for {client_id, _user_info} <- clients, pid <- state.subscribers do
      send(pid, {:client_join, client_id})
    end

    state = update_in(state.clients, &Enum.into(clients, &1))

    {:noreply, state}
  end

  def handle_cast({:unregister_clients, client_ids}, state) do
    for client_id <- client_ids, pid <- state.subscribers do
      send(pid, {:client_leave, client_id})
    end

    state =
      update_in(state.clients, fn clients ->
        Enum.reduce(client_ids, clients, &Map.delete(&2, &1))
      end)

    {:noreply, state}
  end

  def handle_cast({:monitor_clients, pid}, state) do
    Process.monitor(pid)
    state = update_in(state.subscribers, &MapSet.put(&1, pid))

    for {client_id, _user_info} <- state.clients do
      send(pid, {:client_join, client_id})
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    state = update_in(state.subscribers, &MapSet.delete(&1, pid))
    {:noreply, state}
  end
end
