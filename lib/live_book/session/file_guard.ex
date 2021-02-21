defmodule LiveBook.Session.FileGuard do
  @moduledoc false

  # Serves as a locking mechanism for notebook files.
  #
  # Every session process willing to persist notebook
  # should turn to `FileGuard` to make sure the path
  # is not already used by another session.

  use GenServer

  @type state :: %{
          path_with_owner: %{String.t() => owner()}
        }

  @type owner :: %{pid: pid(), monitor_ref: reference()}

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
    {:ok, %{path_with_owner: %{}}}
  end

  @impl true
  def handle_call({:lock, path, owner_pid}, _from, state) do
    if Map.has_key?(state.path_with_owner, path) do
      {:reply, {:error, :already_in_use}, state}
    else
      monitor_ref = Process.monitor(owner_pid)
      owner = %{pid: owner_pid, monitor_ref: monitor_ref}
      state = put_in(state.path_with_owner[path], owner)
      {:reply, :ok, state}
    end
  end

  @impl true
  def handle_cast({:unlock, path, owner_pid}, state) do
    state =
      if Map.has_key?(state.path_with_owner, path) and
           state.path_with_owner[path].pid == owner_pid do
        owner = state.path_with_owner[path]
        Process.demonitor(owner.monitor_ref)
        {_, state} = pop_in(state.path_with_owner[path])
        state
      else
        state
      end

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, _}, state) do
    state = %{
      state
      | path_with_owner:
          for(
            {path, owner} <- state.path_with_owner,
            owner.monitor_ref != ref,
            into: %{},
            do: {path, owner}
          )
    }

    {:noreply, state}
  end
end
