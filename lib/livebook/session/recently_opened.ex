defmodule Livebook.Session.RecentlyOpened do
  @moduledoc false

  use GenServer

  alias Livebook.Session
  alias Livebook.Storage

  @namespace :recently_opened_sessions
  @max_size 10

  @type state :: %{
          required(Session.id()) => %{
            session: Session.t(),
            latest_opened_at: DateTime.t()
          }
        }

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def list_sessions() do
    GenServer.call(__MODULE__, :list_sessions)
  end

  @spec save_session(Session.t()) :: :ok
  def save_session(session) do
    GenServer.cast(__MODULE__, {:save_session, session, DateTime.utc_now()})
  end

  @spec delete_session(Session.id()) :: :ok
  def delete_session(session_id) do
    GenServer.cast(__MODULE__, {:delete_session, session_id})
  end

  @impl true
  def init(_) do
    state =
      Storage.all(@namespace)
      |> Map.new(&{&1.id, %{session: &1.session, latest_opened_at: &1.latest_opened_at}})

    {:ok, state}
  end

  @impl true
  def handle_call(:list_sessions, _from, state) do
    sessions = Enum.map(state, fn {_id, info} -> info.session end)
    {:reply, sessions, state}
  end

  @impl true
  def handle_cast({:save_session, session, latest_opened_at}, state) do
    session_info = %{session: session, latest_opened_at: latest_opened_at}
    Storage.insert(@namespace, session.id, session_info)
    new_state = Map.put(state, session.id, session_info)

    stale_session_id =
      case new_state do
        state when map_size(state) <= @max_size ->
          nil

        state ->
          [{id, _} | _] = Enum.sort_by(state, fn {_id, info} -> info.latest_opened_at end)
          id
      end

    Storage.delete(@namespace, stale_session_id)
    {:noreply, Map.delete(new_state, stale_session_id)}
  end

  @impl true
  def handle_cast({:delete_session, session_id}, state) do
    Storage.delete(@namespace, session_id)
    new_state = Map.delete(state, session_id)
    {:noreply, new_state}
  end
end
