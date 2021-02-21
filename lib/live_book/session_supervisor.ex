defmodule LiveBook.SessionSupervisor do
  @moduledoc false

  # Supervisor responsible for managing running notebook sessions.
  #
  # Allows for creating new session processes on demand
  # and managing them using random ids.

  use DynamicSupervisor

  alias LiveBook.{Session, Utils}

  @name __MODULE__

  def start_link(opts \\ []) do
    DynamicSupervisor.start_link(__MODULE__, opts, name: @name)
  end

  @impl true
  def init(_opts) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  @doc """
  Spawns a new `Session` process with the given options.

  Broadcasts `{:session_created, id}` message under the `"sessions"` topic.
  """
  @spec create_session(keyword()) :: {:ok, Session.id()} | {:error, any()}
  def create_session(opts \\ []) do
    id = Utils.random_id()

    opts = Keyword.put(opts, :id, id)

    case DynamicSupervisor.start_child(@name, {Session, opts}) do
      {:ok, _} ->
        broadcast_sessions_message({:session_created, id})
        {:ok, id}

      {:ok, _, _} ->
        broadcast_sessions_message({:session_created, id})
        {:ok, id}

      :ignore ->
        {:error, :ignore}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Synchronously stops a session process identified by the given id.

  Broadcasts `{:session_delete, id}` message under the `"sessions"` topic.
  """
  @spec delete_session(Session.id()) :: :ok
  def delete_session(id) do
    Session.stop(id)
    broadcast_sessions_message({:session_deleted, id})
    :ok
  end

  defp broadcast_sessions_message(message) do
    Phoenix.PubSub.broadcast(LiveBook.PubSub, "sessions", message)
  end

  @doc """
  Returns ids of all the running session processes.
  """
  @spec get_session_ids() :: list(Session.id())
  def get_session_ids() do
    :global.registered_names()
    |> Enum.flat_map(fn
      {:session, id} -> [id]
      _ -> []
    end)
  end

  @doc """
  Returns summaries of all the running session processes.
  """
  @spec get_session_summaries() :: list(Session.summary())
  def get_session_summaries() do
    Enum.map(get_session_ids(), &Session.get_summary/1)
  end

  @doc """
  Checks if a session process with the given id exists.
  """
  @spec session_exists?(Session.id()) :: boolean()
  def session_exists?(id) do
    :global.whereis_name({:session, id}) != :undefined
  end

  @doc """
  Retrieves pid of a session process identified by the given id.
  """
  @spec get_session_pid(Session.id()) :: {:ok, pid()} | {:error, :nonexistent}
  def get_session_pid(id) do
    case :global.whereis_name({:session, id}) do
      :undefined -> {:error, :nonexistent}
      pid -> {:ok, pid}
    end
  end
end
