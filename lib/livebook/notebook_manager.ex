defmodule Livebook.NotebookManager do
  @moduledoc false

  use GenServer

  alias Livebook.Storage
  alias Livebook.FileSystem

  @namespace :notebook_manager
  @entity_id "global"

  @recent_limit 9

  @type state :: %{
          recent_notebooks: list(notebook_info()),
          starred_notebooks: list(notebook_info())
        }

  @type notebook_info :: %{
          file: FileSystem.File.t(),
          name: String.t(),
          added_at: DateTime.t()
        }

  @doc false
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Returns the list of recent notebooks.
  """
  @spec recent_notebooks() :: list(notebook_info())
  def recent_notebooks() do
    GenServer.call(__MODULE__, :recent_notebooks)
  end

  @doc """
  Returns the list of starred notebooks.
  """
  @spec starred_notebooks() :: list(notebook_info())
  def starred_notebooks() do
    GenServer.call(__MODULE__, :starred_notebooks)
  end

  @doc """
  Subscribes to recent notebooks changes.

  ## Messages

    * `{:recent_notebooks_updated, recent_notebooks}`

  """
  @spec subscribe_recent_notebooks() :: :ok
  def subscribe_recent_notebooks() do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "notebook_manager:recent_notebooks")
  end

  @doc """
  Subscribes to starred notebooks changes.

  ## Messages

    * `{:starred_notebooks_updated, starred_notebooks}`

  """
  @spec subscribe_starred_notebooks() :: :ok
  def subscribe_starred_notebooks() do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "notebook_manager:starred_notebooks")
  end

  @doc """
  Stores the given file as a recent notebook.
  """
  @spec add_recent_notebook(FileSystem.File.t(), String.t()) :: :ok
  def add_recent_notebook(file, name) do
    GenServer.cast(__MODULE__, {:add_recent_notebook, file, name})
  end

  @doc """
  Stores the given file as a starred notebook.
  """
  @spec add_starred_notebook(FileSystem.File.t(), String.t()) :: :ok
  def add_starred_notebook(file, name) do
    GenServer.cast(__MODULE__, {:add_starred_notebook, file, name})
  end

  @doc """
  Removes the given file from recent notebooks.
  """
  @spec remove_recent_notebook(FileSystem.File.t()) :: :ok
  def remove_recent_notebook(file) do
    GenServer.cast(__MODULE__, {:remove_recent_notebook, file})
  end

  @doc """
  Removes the given file from starred notebooks.
  """
  @spec remove_starred_notebook(FileSystem.File.t()) :: :ok
  def remove_starred_notebook(file) do
    GenServer.cast(__MODULE__, {:remove_starred_notebook, file})
  end

  @doc """
  Clears all information about notebooks from the removed file system.
  """
  @spec remove_file_system(Livebook.Utils.id()) :: :ok
  def remove_file_system(file_system_id) do
    GenServer.cast(__MODULE__, {:remove_file_system, file_system_id})
  end

  @doc """
  Updates the tracked notebook name for the given file.

  We track notebook names, so that we don't need to read and parse
  notebooks whenever we want to show the tracked entries. Consequently,
  we need to update the tracked name whenever it changes.
  """
  @spec update_notebook_name(FileSystem.File.t(), String.t()) :: :ok
  def update_notebook_name(file, name) do
    GenServer.cast(__MODULE__, {:update_notebook_name, file, name})
  end

  @impl true
  def init(_opts) do
    {:ok, nil, {:continue, :load_state}}
  end

  @impl true
  def handle_continue(:load_state, nil) do
    {:noreply, load_state()}
  end

  def handle_continue(:dump_state, state) do
    dump_state(state)
    {:noreply, state}
  end

  @impl true
  def handle_call(:recent_notebooks, _from, state) do
    {:reply, state.recent_notebooks, state}
  end

  def handle_call(:starred_notebooks, _from, state) do
    {:reply, state.starred_notebooks, state}
  end

  @impl true
  def handle_cast({:add_recent_notebook, file, name}, state = prev_state) do
    recent_notebooks = Enum.reject(state.recent_notebooks, &(&1.file == file))

    recent_notebooks = [
      %{file: file, name: name, added_at: DateTime.utc_now()} | recent_notebooks
    ]

    recent_notebooks = Enum.take(recent_notebooks, @recent_limit)

    # Theoretically the starred name we load may be outdated if it was
    # modified outside Livebook, so whenever a new recent notebook is
    # added we update the starred entry if any
    starred_notebooks = update_notebook_names(state.starred_notebooks, file, name)

    state = %{state | recent_notebooks: recent_notebooks, starred_notebooks: starred_notebooks}
    broadcast_changes(state, prev_state)
    {:noreply, state, {:continue, :dump_state}}
  end

  def handle_cast({:add_starred_notebook, file, name}, state = prev_state) do
    if Enum.any?(state.starred_notebooks, &(&1.file == file)) do
      {:noreply, state}
    else
      starred_notebooks = [
        %{file: file, name: name, added_at: DateTime.utc_now()} | state.starred_notebooks
      ]

      state = %{state | starred_notebooks: starred_notebooks}
      broadcast_changes(state, prev_state)
      {:noreply, state, {:continue, :dump_state}}
    end
  end

  def handle_cast({:remove_recent_notebook, file}, state = prev_state) do
    recent_notebooks = Enum.reject(state.recent_notebooks, &(&1.file == file))
    state = %{state | recent_notebooks: recent_notebooks}
    broadcast_changes(state, prev_state)
    {:noreply, state, {:continue, :dump_state}}
  end

  def handle_cast({:remove_starred_notebook, file}, state = prev_state) do
    starred_notebooks = Enum.reject(state.starred_notebooks, &(&1.file == file))
    state = %{state | starred_notebooks: starred_notebooks}
    broadcast_changes(state, prev_state)
    {:noreply, state, {:continue, :dump_state}}
  end

  def handle_cast({:remove_file_system, file_system_id}, state = prev_state) do
    recent_notebooks = remove_notebooks_on_file_system(state.recent_notebooks, file_system_id)
    starred_notebooks = remove_notebooks_on_file_system(state.starred_notebooks, file_system_id)
    state = %{state | recent_notebooks: recent_notebooks, starred_notebooks: starred_notebooks}
    broadcast_changes(state, prev_state)
    {:noreply, state, {:continue, :dump_state}}
  end

  def handle_cast({:update_notebook_name, file, name}, state = prev_state) do
    recent_notebooks = update_notebook_names(state.recent_notebooks, file, name)
    starred_notebooks = update_notebook_names(state.starred_notebooks, file, name)
    state = %{state | recent_notebooks: recent_notebooks, starred_notebooks: starred_notebooks}
    broadcast_changes(state, prev_state)
    {:noreply, state, {:continue, :dump_state}}
  end

  defp remove_notebooks_on_file_system(notebook_infos, file_system_id) do
    Enum.reject(notebook_infos, &(&1.file.file_system.id == file_system_id))
  end

  defp update_notebook_names(notebook_infos, file, name) do
    Enum.map(notebook_infos, fn
      %{file: ^file} = info -> %{info | name: name}
      info -> info
    end)
  end

  defp broadcast_changes(state, prev_state) do
    if state.recent_notebooks != prev_state.recent_notebooks do
      Phoenix.PubSub.broadcast(
        Livebook.PubSub,
        "notebook_manager:recent_notebooks",
        {:recent_notebooks_updated, state.recent_notebooks}
      )
    end

    if state.starred_notebooks != prev_state.starred_notebooks do
      Phoenix.PubSub.broadcast(
        Livebook.PubSub,
        "notebook_manager:starred_notebooks",
        {:starred_notebooks_updated, state.starred_notebooks}
      )
    end
  end

  defp load_state() do
    attrs =
      case Storage.fetch(@namespace, @entity_id) do
        {:ok, attrs} -> attrs
        _ -> %{}
      end

    file_system_by_id =
      for file_system <- Livebook.Settings.file_systems(),
          do: {file_system.id, file_system},
          into: %{}

    %{
      recent_notebooks: load_notebook_infos(attrs[:recent_notebooks], file_system_by_id),
      starred_notebooks: load_notebook_infos(attrs[:starred_notebooks], file_system_by_id)
    }
  end

  defp load_notebook_infos(nil, _file_system_by_id), do: []

  defp load_notebook_infos(notebook_infos, file_system_by_id) do
    for %{file: file, name: name, added_at: added_at} <- notebook_infos,
        file = load_file(file, file_system_by_id),
        added_at = load_datetime(added_at) do
      %{file: file, name: name, added_at: added_at}
    end
  end

  defp load_file(%{file_system_id: file_system_id, path: path}, file_system_by_id) do
    if file_system = file_system_by_id[file_system_id] do
      %FileSystem.File{file_system: file_system, path: path}
    end
  end

  defp load_datetime(datetime) do
    DateTime.from_unix!(datetime, :microsecond)
  end

  defp dump_state(state) do
    attrs = [
      recent_notebooks: dump_notebooks_infos(state.recent_notebooks),
      starred_notebooks: dump_notebooks_infos(state.starred_notebooks)
    ]

    Storage.insert(@namespace, @entity_id, attrs)
  end

  defp dump_notebooks_infos(notebook_infos) do
    for info <- notebook_infos do
      %{
        file: dump_file(info.file),
        name: info.name,
        added_at: DateTime.to_unix(info.added_at, :microsecond)
      }
    end
  end

  defp dump_file(file) do
    %{file_system_id: file.file_system.id, path: file.path}
  end
end
