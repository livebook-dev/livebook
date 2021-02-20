defmodule LiveBook.Session.FileGuard do
  @moduledoc false

  # Serves as a locking mechanism for notebook files.
  #
  # Every session process willing to persist notebook
  # should turn to `FileGuard` to make sure the path
  # is not already used by another session.

  use GenServer

  @type state :: %{
          owner_with_monitor: %{pid() => reference()},
          path_with_owner: %{String.t() => pid()}
        }

  @name __MODULE__

  def start_link(_opts \\ []) do
    GenServer.start_link(__MODULE__, [], name: @name)
  end

  def stop() do
    GenServer.stop(@name)
  end

  @doc """
  Locks the given file associating it with the given process.

  If the owner process dies the file is automatically unlocked.
  """
  @spec lock(String.t(), pid()) :: :ok | {:error, :already_in_use}
  def lock(path, owner_pid) do
    GenServer.call(@name, {:lock, path, owner_pid})
  end

  @doc """
  Unlocks the given file if the given process is the one that locked it.
  """
  @spec unlock(String.t(), pid()) :: :ok
  def unlock(path, owner_pid) do
    GenServer.cast(@name, {:unlock, path, owner_pid})
  end

  # Callbacks

  @impl true
  def init(_opts) do
    {:ok, %{owner_with_monitor: %{}, path_with_owner: %{}}}
  end

  @impl true
  def handle_call({:lock, path, owner_pid}, _from, state) do
    if Map.has_key?(state.path_with_owner, path) do
      {:reply, {:error, :already_in_use}, state}
    else
      state = %{state | path_with_owner: Map.put(state.path_with_owner, path, owner_pid)}
      state = maybe_monitor(state, owner_pid)
      {:reply, :ok, state}
    end
  end

  @impl true
  def handle_cast({:unlock, path, owner_pid}, state) do
    state =
      if {path, owner_pid} in state.path_with_owner do
        state = %{state | path_with_owner: Map.delete(state.path_with_owner, path)}
        maybe_demonitor(state, owner_pid)
      else
        state
      end

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _, :process, pid, _}, state) do
    state = %{
      state
      | path_with_owner:
          for(
            {path, owner_pid} <- state.path_with_owner,
            owner_pid != pid,
            into: %{},
            do: {path, owner_pid}
          )
    }

    state = %{state | owner_with_monitor: Map.delete(state.owner_with_monitor, pid)}
    {:noreply, state}
  end

  defp maybe_monitor(state, owner_pid) do
    if Map.has_key?(state.owner_with_monitor, owner_pid) do
      state
    else
      monitor_ref = Process.monitor(owner_pid)
      %{state | owner_with_monitor: Map.put(state.owner_with_monitor, owner_pid, monitor_ref)}
    end
  end

  defp maybe_demonitor(state, owner_pid) do
    if owner_pid in Map.values(state.path_with_owner) do
      state
    else
      monitor_ref = state.owner_with_monitor[owner_pid]
      Process.demonitor(monitor_ref)
      %{state | owner_with_monitor: Map.delete(state.owner_with_monitor, owner_pid)}
    end
  end
end
