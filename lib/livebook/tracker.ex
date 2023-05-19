defmodule Livebook.Tracker do
  @moduledoc false

  use Phoenix.Tracker

  alias Livebook.Session
  alias Livebook.App

  @name __MODULE__

  def start_link(opts \\ []) do
    opts = Keyword.merge([name: @name], opts)
    Phoenix.Tracker.start_link(__MODULE__, opts, opts)
  end

  @sessions_topic "sessions"
  @apps_topic "apps"

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

  @doc """
  Starts tracking the given app, making it visible globally.
  """
  @spec track_app(App.t()) :: :ok | {:error, any()}
  def track_app(app) do
    case Phoenix.Tracker.track(@name, app.pid, @apps_topic, app.slug, %{app: app}) do
      {:ok, _ref} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Updates the tracked app object matching the given slug.
  """
  @spec update_app(App.t()) :: :ok | {:error, any()}
  def update_app(app) do
    case Phoenix.Tracker.update(@name, app.pid, @apps_topic, app.slug, %{app: app}) do
      {:ok, _ref} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Returns all tracked apps.
  """
  @spec list_apps() :: list(App.t())
  def list_apps() do
    presences = Phoenix.Tracker.list(@name, @apps_topic)
    for {_slug, %{app: app}} <- presences, do: app
  end

  @doc """
  Returns tracked app with the given slug.
  """
  @spec fetch_app(String.t()) :: {:ok, App.t()} | :error
  def fetch_app(slug) do
    case Phoenix.Tracker.get_by_key(@name, @apps_topic, slug) do
      [{_slug, %{app: app}}] -> {:ok, app}
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
    joins = Map.new(joins)
    leaves = Map.new(leaves)

    messages =
      for id <- Enum.uniq(Map.keys(joins) ++ Map.keys(leaves)) do
        case {joins[id], leaves[id]} do
          {%{session: session}, nil} -> {:session_created, session}
          {nil, %{session: session}} -> {:session_closed, session}
          {%{session: session}, %{}} -> {:session_updated, session}
        end
      end

    for message <- messages do
      Phoenix.PubSub.direct_broadcast!(
        state.node_name,
        state.pubsub_server,
        "tracker_sessions",
        message
      )
    end
  end

  defp handle_topic_diff(@apps_topic, {joins, leaves}, state) do
    joins = Map.new(joins)
    leaves = Map.new(leaves)

    messages =
      for slug <- Enum.uniq(Map.keys(joins) ++ Map.keys(leaves)) do
        case {joins[slug], leaves[slug]} do
          {%{app: app}, nil} -> {:app_created, app}
          {nil, %{app: app}} -> {:app_closed, app}
          {%{app: app}, %{}} -> {:app_updated, app}
        end
      end

    for message <- messages do
      Phoenix.PubSub.direct_broadcast!(
        state.node_name,
        state.pubsub_server,
        "tracker_apps",
        message
      )
    end
  end
end
