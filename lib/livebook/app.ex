defmodule Livebook.App do
  @moduledoc false

  # Process corresponding to a deployed app, orchestrating app sessions.
  #
  # An app process is identified by a user-defined slug, which also
  # determines its URL. The process starts when the first notebook is
  # deployed under this slug. Subsequent notebook deployments for the
  # same slug are handled by the process directly.
  #
  # App is configured via `%Livebook.Notebook.AppSettings{}` in the
  # deployed notebook. Attributes specifying app-level behaviour are
  # always taken from the most recently deployed notebook (e.g. access
  # type, automatic shutdown, deployment strategy).

  defstruct [
    :slug,
    :pid,
    :version,
    :warnings,
    :notebook_name,
    :public?,
    :multi_session,
    :sessions
  ]

  @type t :: %{
          slug: slug(),
          pid: pid(),
          version: pos_integer(),
          warnings: list(String.t()),
          notebook_name: String.t(),
          public?: boolean(),
          multi_session: boolean(),
          sessions: list(app_session())
        }

  @type slug :: String.t()

  @type app_session :: %{
          id: Livebook.Utils.id(),
          pid: pid(),
          version: pos_integer(),
          created_at: DateTime.t(),
          app_status: Livebook.Session.Data.app_status(),
          client_count: non_neg_integer(),
          started_by: Livebook.Users.User.t() | nil
        }

  use GenServer, restart: :temporary

  @doc """
  Starts an apps process.

  ## Options

    * `:notebook` (required) - the notebook for initial deployment

    * `:warnings` - a list of warnings to show for the initial deployment

    * `:files_source` - a location to fetch notebook files from, see
      `Livebook.Session.start_link/1` for more details

  """
  @spec start_link(keyword()) :: {:ok, pid} | {:error, any()}
  def start_link(opts) do
    notebook = Keyword.fetch!(opts, :notebook)
    warnings = Keyword.get(opts, :warnings, [])
    files_source = Keyword.get(opts, :files_source)

    GenServer.start_link(__MODULE__, {notebook, warnings, files_source})
  end

  @doc """
  Gets app information.
  """
  @spec get_by_pid(pid()) :: t()
  def get_by_pid(pid) do
    GenServer.call(pid, :describe_self)
  end

  @doc """
  Gets app settings.

  Note that the settings are always taken from the most recently
  deployed notebook.
  """
  @spec get_settings(pid()) :: Livebook.Notebook.AppSettings.t()
  def get_settings(pid) do
    GenServer.call(pid, :get_settings)
  end

  @doc """
  Returns an app session id.

  For multi-session app, this always creates a new session.

  For single-session app, this returns an existing session if one
  exists, otherwise creating a new one. If zero-downtime deployment
  is enabled, an old session may be returned unless the new session
  is fully executed.

  ## Options

    * `:user` - the user requesting the session. In multi-session app,
      we track who starts each session

  """
  @spec get_session_id(pid(), keyword()) :: Livebook.Session.id()
  def get_session_id(pid, opts \\ []) do
    opts = Keyword.validate!(opts, [:user])
    user = opts[:user]

    GenServer.call(pid, {:get_session_id, user})
  end

  @doc """
  Deploys a new notebook into the app.
  """
  @spec deploy(pid(), Livebook.Notebook.t(), keyword()) :: :ok
  def deploy(pid, notebook, opts \\ []) do
    warnings = Keyword.get(opts, :warnings, [])
    files_source = Keyword.get(opts, :files_source)
    GenServer.cast(pid, {:deploy, notebook, warnings, files_source})
  end

  @doc """
  Closes the app.

  This operation results in all app sessions being closed as well.
  """
  def close(pid) do
    GenServer.call(pid, :close)
  end

  @doc """
  Subscribes to app messages.

  ## Messages

    * `{:app_updated, app}`

  """
  @spec subscribe(slug()) :: :ok | {:error, term()}
  def subscribe(slug) do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "apps:#{slug}")
  end

  @doc """
  Unsubscribes from app messages.
  """
  @spec unsubscribe(slug()) :: :ok | {:error, term()}
  def unsubscribe(slug) do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "apps:#{slug}")
  end

  @impl true
  def init({notebook, warnings, files_source}) do
    {:ok,
     %{
       version: 1,
       notebook: notebook,
       files_source: files_source,
       warnings: warnings,
       sessions: [],
       users: %{}
     }
     |> start_eagerly()}
  end

  @impl true
  def handle_call(:describe_self, _from, state) do
    {:reply, self_from_state(state), state}
  end

  def handle_call({:get_session_id, user}, _from, state) do
    {session_id, state} =
      case {state.notebook.app_settings.multi_session, single_session_app_session(state)} do
        {false, %{} = app_session} ->
          {app_session.id, state}

        {multi_session, _} ->
          user = if(multi_session, do: user)
          {:ok, state, app_session} = start_app_session(state, user)
          {app_session.id, notify_update(state)}
      end

    {:reply, session_id, state}
  end

  def handle_call(:get_settings, _from, state) do
    {:reply, state.notebook.app_settings, state}
  end

  def handle_call(:close, _from, state) do
    {:stop, :shutdown, :ok, state}
  end

  @impl true
  def handle_cast({:deploy, notebook, warnings, files_source}, state) do
    true = notebook.app_settings.slug == state.notebook.app_settings.slug

    {:noreply,
     %{
       state
       | notebook: notebook,
         version: state.version + 1,
         warnings: warnings,
         files_source: files_source
     }
     |> start_eagerly()
     |> shutdown_old_versions()
     |> notify_update()}
  end

  @impl true
  def handle_info({:app_status_changed, session_id, status}, state) do
    state = update_app_session(state, session_id, &%{&1 | app_status: status})
    {:noreply, state |> shutdown_old_versions() |> notify_update()}
  end

  def handle_info({:app_client_count_changed, session_id, client_count}, state) do
    state = update_app_session(state, session_id, &%{&1 | client_count: client_count})
    {:noreply, notify_update(state)}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    app_session = Enum.find(state.sessions, &(&1.pid == pid))
    state = update_in(state.sessions, &(&1 -- [app_session]))
    {:noreply, notify_update(state)}
  end

  def handle_info({:user_change, user}, state) do
    state = put_in(state.users[user.id].user, user)
    {:noreply, notify_update(state)}
  end

  defp self_from_state(state) do
    %{
      slug: state.notebook.app_settings.slug,
      pid: self(),
      version: state.version,
      warnings: state.warnings,
      notebook_name: state.notebook.name,
      public?: state.notebook.app_settings.access_type == :public,
      multi_session: state.notebook.app_settings.multi_session,
      sessions: state.sessions
    }
  end

  defp single_session_app_session(state) do
    app_session = Enum.find(state.sessions, &(&1.version == state.version))

    if app_session do
      if state.notebook.app_settings.zero_downtime and not status_ready?(app_session.app_status) do
        Enum.find(state.sessions, &status_ready?(&1.app_status))
      end || app_session
    end
  end

  defp status_ready?(%{execution: :executed, lifecycle: :active}), do: true
  defp status_ready?(_status), do: false

  defp start_eagerly(state) when state.notebook.app_settings.multi_session, do: state

  defp start_eagerly(state) do
    if temporary_sessions?(state.notebook.app_settings) do
      state
    else
      {:ok, state, _app_session} = start_app_session(state)
      state
    end
  end

  defp start_app_session(state, user \\ nil) do
    user = if(state.notebook.teams_enabled, do: user)

    opts = [
      notebook: state.notebook,
      files_source: state.files_source,
      mode: :app,
      app_pid: self(),
      auto_shutdown_ms: state.notebook.app_settings.auto_shutdown_ms,
      started_by: user
    ]

    case Livebook.Sessions.create_session(opts) do
      {:ok, session} ->
        app_session = %{
          id: session.id,
          pid: session.pid,
          version: state.version,
          created_at: session.created_at,
          app_status: %{execution: :executing, lifecycle: :active},
          client_count: 0,
          started_by: user
        }

        Process.monitor(session.pid)

        state = update_in(state.sessions, &[app_session | &1])

        {:ok, state, app_session}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp update_app_session(state, session_id, fun) do
    update_in(state.sessions, fn sessions ->
      Enum.map(sessions, fn
        %{id: ^session_id} = session -> fun.(session)
        session -> session
      end)
    end)
  end

  defp temporary_sessions?(app_settings), do: app_settings.auto_shutdown_ms != nil

  defp shutdown_old_versions(state) when not state.notebook.app_settings.multi_session do
    single_session_app_session = single_session_app_session(state)

    for app_session <- state.sessions,
        app_session != single_session_app_session,
        app_session.version < state.version do
      shutdown_session(app_session)
    end

    state
  end

  defp shutdown_old_versions(state), do: state

  defp shutdown_session(app_session) do
    if app_session.app_status.lifecycle == :active do
      Livebook.Session.app_shutdown(app_session.pid)
    end
  end

  defp notify_update(state) do
    app = self_from_state(state)
    Livebook.Apps.update_app(app)
    broadcast_message(state.notebook.app_settings.slug, {:app_updated, app})
    state
  end

  defp broadcast_message(slug, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "apps:#{slug}", message)
  end
end
