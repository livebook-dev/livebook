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
          file_with_owner_ref: %{FileSystem.File.t() => reference()}
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
    {:ok, %{file_with_owner_ref: %{}}}
  end

  @impl true
  def handle_call({:lock, file, owner_pid}, _from, state) do
    if Map.has_key?(state.file_with_owner_ref, file) do
      {:reply, {:error, :already_in_use}, state}
    else
      monitor_ref = Process.monitor(owner_pid)
      state = put_in(state.file_with_owner_ref[file], monitor_ref)
      {:reply, :ok, state}
    end
  end

  @impl true
  def handle_cast({:unlock, file}, state) do
    {maybe_ref, state} = pop_in(state.file_with_owner_ref[file])
    maybe_ref && Process.demonitor(maybe_ref, [:flush])

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, _}, state) do
    {file, ^ref} = Enum.find(state.file_with_owner_ref, &(elem(&1, 1) == ref))
    {_, state} = pop_in(state.file_with_owner_ref[file])
    {:noreply, state}
  end
end
