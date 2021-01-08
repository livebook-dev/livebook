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
  Spawns a new session process.

  Broadcasts `{:session_created, id}` message under the `"sessions"` topic.
  """
  @spec create_session() :: {:ok, Session.session_id()} | {:error, any()}
  def create_session() do
    id = Utils.random_id()

    case DynamicSupervisor.start_child(@name, {Session, id}) do
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
  @spec delete_session(Session.session_id()) :: :ok
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
  @spec get_session_ids() :: list(Session.session_id())
  def get_session_ids() do
    :global.registered_names()
    |> Enum.flat_map(fn
      {:session, id} -> [id]
      _ -> []
    end)
  end

  @doc """
  Checks if a session process with the given id exists.
  """
  @spec session_exists?(Session.session_id()) :: boolean()
  def session_exists?(id) do
    :global.whereis_name({:session, id}) != :undefined
  end
end
