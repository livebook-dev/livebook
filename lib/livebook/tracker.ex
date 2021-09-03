defmodule Livebook.Tracker do
  @moduledoc false

  use Phoenix.Tracker

  alias Livebook.Session

  @name __MODULE__

  def start_link(opts \\ []) do
    opts = Keyword.merge([name: @name], opts)
    Phoenix.Tracker.start_link(__MODULE__, opts, opts)
  end

  @sessions_topic "sessions"

  @doc """
  Starts tracking the given session, making it visible globally.
  """
  @spec track_session(Session.t()) :: :ok | {:error, any()}
  def track_session(session) do
    case Phoenix.Tracker.track(@name, session.pid, @sessions_topic, session.id, %{
           session: session
         }) do
      {:ok, _ref} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Updates the tracked session object matching the given id.
  """
  @spec update_session(Session.t()) :: :ok | {:error, any()}
  def update_session(session) do
    case Phoenix.Tracker.update(@name, session.pid, @sessions_topic, session.id, %{
           session: session
         }) do
      {:ok, _ref} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Returns all tracked sessions.
  """
  @spec list_sessions() :: list(Session.t())
  def list_sessions() do
    presences = Phoenix.Tracker.list(@name, @sessions_topic)
    for {_id, %{session: session}} <- presences, do: session
  end

  @doc """
  Returns tracked session with the given id.
  """
  @spec fetch_session(Session.id()) :: {:ok, Session.t()} | :error
  def fetch_session(id) do
    case Phoenix.Tracker.get_by_key(@name, @sessions_topic, id) do
      [{_id, %{session: session}}] -> {:ok, session}
      _ -> :error
    end
  end

  @impl true
  def init(opts) do
    server = Keyword.fetch!(opts, :pubsub_server)
    {:ok, %{pubsub_server: server, node_name: Phoenix.PubSub.node_name(server)}}
  end

  @impl true
  def handle_diff(diff, state) do
    for {topic, topic_diff} <- diff do
      handle_topic_diff(topic, topic_diff, state)
    end

    {:ok, state}
  end

  defp handle_topic_diff(@sessions_topic, {joins, leaves}, state) do
    Enum.group_by(
      Enum.map(leaves, &Tuple.append(&1, :leave)) ++ Enum.map(joins, &Tuple.append(&1, :join)),
      &elem(&1, 0)
    )
    |> Enum.map(fn
      {id, [{id, %{session: session}, :leave}]} ->
        {:session_closed, session}

      {id, [{id, %{session: session}, :join}]} ->
        {:session_created, session}

      {id, [{id, _meta, :leave}, {id, %{session: session}, :join}]} ->
        {:session_updated, session}
    end)
    |> Enum.each(fn message ->
      Phoenix.PubSub.direct_broadcast!(state.node_name, state.pubsub_server, "sessions", message)
    end)
  end
end
