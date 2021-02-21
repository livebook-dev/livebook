defmodule LiveBook.Session.FileGuard do
  @moduledoc false

  # Serves as a locking mechanism for notebook files.
  #
  # Every session process willing to persist notebook
  # should turn to `FileGuard` to make sure the path
  # is not already used by another session.

  use GenServer

  @type state :: %{
          path_with_owner_ref: %{String.t() => reference()}
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
  Unlocks the given file.
  """
  @spec unlock(String.t()) :: :ok
  def unlock(path) do
    GenServer.cast(@name, {:unlock, path})
  end

  # Callbacks

  @impl true
  def init(_opts) do
    {:ok, %{path_with_owner_ref: %{}}}
  end

  @impl true
  def handle_call({:lock, path, owner_pid}, _from, state) do
    if Map.has_key?(state.path_with_owner_ref, path) do
      {:reply, {:error, :already_in_use}, state}
    else
      monitor_ref = Process.monitor(owner_pid)
      state = put_in(state.path_with_owner_ref[path], monitor_ref)
      {:reply, :ok, state}
    end
  end

  @impl true
  def handle_cast({:unlock, path}, state) do
    {maybe_ref, state} = pop_in(state.path_with_owner_ref[path])
    maybe_ref && Process.demonitor(maybe_ref, [:flush])

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, _}, state) do
    {path, ^ref} = Enum.find(state.path_with_owner_ref, &(elem(&1, 1) == ref))
    {_, state} = pop_in(state.path_with_owner_ref[path])
    {:noreply, state}
  end
end
