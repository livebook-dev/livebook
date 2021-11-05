defmodule Livebook.Session.FileGuard do
  @moduledoc false

  # Serves as a locking mechanism for notebook files.
  #
  # Every session process willing to persist notebook
  # should turn to `FileGuard` to make sure the file
  # is not already used by another session.

  use GenServer

  alias Livebook.FileSystem

  @type state :: %{
          files: %{term() => {FileSystem.File.t(), owner_pid :: pid(), reference()}}
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
  @spec lock(FileSystem.File.t(), pid()) :: :ok | {:error, :already_in_use}
  def lock(file, owner_pid) do
    GenServer.call(@name, {:lock, file, owner_pid})
  end

  @doc """
  Unlocks the given file.
  """
  @spec unlock(FileSystem.File.t()) :: :ok
  def unlock(file) do
    GenServer.cast(@name, {:unlock, file})
  end

  # Callbacks

  @impl true
  def init(_opts) do
    {:ok, %{files: %{}}}
  end

  @impl true
  def handle_call({:lock, file, owner_pid}, _from, state) do
    file_id = FileSystem.File.resource_identifier(file)

    if Map.has_key?(state.files, file_id) or lock_globally(file, file_id, owner_pid) == false do
      {:reply, {:error, :already_in_use}, state}
    else
      monitor_ref = Process.monitor(owner_pid)
      state = put_in(state.files[file_id], {file, owner_pid, monitor_ref})
      {:reply, :ok, state}
    end
  end

  @impl true
  def handle_cast({:unlock, file}, state) do
    file_id = FileSystem.File.resource_identifier(file)

    {maybe_file, state} = pop_in(state.files[file_id])

    with {file, owner_pid, monitor_ref} <- maybe_file do
      unlock_globally(file, file_id, owner_pid)
      Process.demonitor(monitor_ref, [:flush])
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, _}, state) do
    {file_id, {file, owner_pid, ^ref}} =
      Enum.find(state.files, &match?({_file_id, {_file, _owner_pid, ^ref}}, &1))

    unlock_globally(file, file_id, owner_pid)
    {_, state} = pop_in(state.files[file_id])

    {:noreply, state}
  end

  defp lock_globally(file, file_id, owner_pid) do
    FileSystem.File.local?(file) or :global.set_lock({file_id, owner_pid})
  end

  defp unlock_globally(file, file_id, owner_pid) do
    FileSystem.File.local?(file) or :global.del_lock({file_id, owner_pid})
  end
end
