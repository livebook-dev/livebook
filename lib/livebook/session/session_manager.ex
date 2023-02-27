defmodule Livebook.Session.SessionManager do
  @moduledoc false

  use GenServer

  alias Livebook.Storage

  @namespace :session_manager
  @entity_id "recently_opened"
  @max_size 10

  @type state :: list(file_path :: String.t())

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def get_recently_opened_sessions() do
    GenServer.call(__MODULE__, :get_recently_opened_sessions)
  end

  @spec save_recently_opened_sessions(String.t()) :: :ok
  def save_recently_opened_sessions(session_path) do
    GenServer.cast(__MODULE__, {:save_recently_opened_sessions, session_path})
  end

  @spec delete_recently_opened_sessions(String.t()) :: :ok
  def delete_recently_opened_sessions(session_path) do
    GenServer.cast(__MODULE__, {:delete_recently_opened_sessions, session_path})
  end

  @impl true
  def init(_) do
    state =
      case Storage.fetch(@namespace, @entity_id) do
        {:ok, %{file_paths: file_paths}} -> file_paths
        _ -> []
      end

    {:ok, state}
  end

  @impl true
  def handle_call(:get_recently_opened_sessions, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast({:save_recently_opened_sessions, session_path}, state) do
    new_state = [session_path | state -- [session_path]] |> Enum.slice(0, @max_size)
    Storage.insert(@namespace, @entity_id, %{file_paths: new_state})
    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:delete_recently_opened_sessions, session_path}, state) do
    new_state = state -- [session_path]
    Storage.insert(@namespace, @entity_id, %{file_paths: new_state})
    {:noreply, new_state}
  end
end
