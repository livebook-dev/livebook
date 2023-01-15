defmodule Livebook.Session.RecentlyOpened do
  @moduledoc false

  use GenServer

  alias Livebook.Session
  alias Livebook.Users.User
  alias Livebook.Storage

  @namespace :recently_opened_sessions
  @max_size 10

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def list_sessions(user) do
    GenServer.call(__MODULE__, {:list_sessions, user})
  end

  @spec save_session(User.t(), Session.t()) :: :ok
  def save_session(user, session) do
    GenServer.cast(__MODULE__, {:save_session, user, session})
  end

  @spec delete_session(User.t(), Session.id()) :: :ok
  def delete_session(user, session_id) do
    GenServer.cast(__MODULE__, {:delete_session, user, session_id})
  end

  @impl true
  def init(_) do
    state =
      Storage.all(@namespace)
      |> Map.new(&{&1.id, &1.sessions})

    {:ok, state}
  end

  @impl true
  def handle_call({:list_sessions, user}, _from, state) do
    {:reply, Map.get(state, user.id, []), state}
  end

  @impl true
  def handle_cast({:save_session, user, session}, state) do
    new_state =
      Map.update(state, user.id, [session], fn sessions ->
        sessions
        |> Kernel.--([session])
        |> List.insert_at(0, session)
        |> Enum.take(@max_size)
      end)

    Storage.insert(@namespace, user.id, sessions: new_state[user.id])

    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:delete_session, user, session_id}, state) do
    new_state =
      Map.update(state, user.id, [], fn sessions ->
        Enum.reject(sessions, &(&1.id == session_id))
      end)

    Storage.insert(@namespace, user.id, sessions: new_state[user.id])

    {:noreply, new_state}
  end
end
