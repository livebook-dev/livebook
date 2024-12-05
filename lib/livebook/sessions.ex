defmodule Livebook.Sessions do
  # This module is responsible for starting and discovering sessions.
  #
  # Every session has a server process and is described by a `%Session{}`
  # info struct. Information about all sessions in the cluster is
  # propagated using `Livebook.Tracker`, which serves as an ephemeral
  # distributed database for the `%Session{}` structs.

  alias Livebook.{Session, Utils}

  @doc """
  Spawns a new `Session` process with the given options.
  """
  @spec create_session(keyword()) :: {:ok, Session.t()} | {:error, any()}
  def create_session(opts \\ []) do
    id = Utils.random_node_aware_id()

    opts = Keyword.put(opts, :id, id)

    case DynamicSupervisor.start_child(Livebook.SessionSupervisor, {Session, opts}) do
      {:ok, _pid, session} ->
        {:ok, session}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Returns all the running sessions.
  """
  @spec list_sessions() :: list(Session.t())
  def list_sessions() do
    Livebook.Tracker.list_sessions()
  end

  @doc """
  Returns tracked session with the given id.
  """
  @spec fetch_session(Session.id()) ::
          {:ok, Session.t()} | {:error, :not_found | :different_boot_id}
  def fetch_session(id) do
    case Livebook.Tracker.fetch_session(id) do
      {:ok, session} ->
        {:ok, session}

      :error ->
        boot_id = Livebook.Config.random_boot_id()

        case Utils.node_from_node_aware_id(id) do
          # The local tracker server doesn't know about this session,
          # but it may not have propagated yet, so we extract the session
          # node from id and ask the corresponding tracker directly
          {:ok, other_node, _other_boot_id} when other_node != node() ->
            case :rpc.call(other_node, Livebook.Tracker, :fetch_session, [id]) do
              {:ok, session} -> {:ok, session}
              _ -> {:error, :not_found}
            end

          {:ok, other_node, other_boot_id}
          when other_node == node() and other_boot_id != boot_id ->
            {:error, :different_boot_id}

          _ ->
            {:error, :not_found}
        end
    end
  end

  @doc """
  Updates the given session info across the cluster.
  """
  @spec update_session(Session.t()) :: :ok | {:error, any()}
  def update_session(session) do
    Livebook.Tracker.update_session(session)
  end

  @doc """
  Subscribes to update in sessions list.

  ## Messages

    * `{:session_created, session}`
    * `{:session_updated, session}`
    * `{:session_closed, session}`

  """
  @spec subscribe() :: :ok | {:error, term()}
  def subscribe() do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "tracker_sessions")
  end
end
