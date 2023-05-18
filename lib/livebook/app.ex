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

  defstruct [:slug, :pid, :version, :notebook_name, :public?, :sessions]

  @type t :: %{
          slug: slug(),
          pid: pid(),
          version: pos_integer(),
          notebook_name: String.t(),
          public?: boolean(),
          sessions: list(app_session())
        }

  @type slug :: String.t()

  @type app_session :: %{
          id: Livebook.Utils.id(),
          pid: pid(),
          version: pos_integer(),
          created_at: DateTime.t(),
          app_status: Livebook.Session.Data.app_status(),
          client_count: non_neg_integer()
        }

  use GenServer, restart: :temporary

  @auto_shutdown_inactivity_ms %{inactive_5s: 5_000, inactive_1m: 60_000, inactive_1h: 3600_000}

  @doc """
  Starts an apps process.

  ## Options

    * `:notebook` (required) - the notebook for initial deployment

    * `:auto_shutdown_inactivity_ms` - mapping from
      `t:Livebook.Notebook.AppSettings.auto_shutdown_type/0` to the
      number of inactivity milliseconds after which automatic shutdown
      should be triggered. This can be used in tests to artificially
      shorten reaction time

  """
  @spec start_link(keyword()) :: {:ok, pid} | {:error, any()}
  def start_link(opts) do
    notebook = Keyword.fetch!(opts, :notebook)

    auto_shutdown_inactivity_ms =
      Keyword.get(opts, :auto_shutdown_inactivity_ms, @auto_shutdown_inactivity_ms)

    GenServer.start_link(__MODULE__, {notebook, auto_shutdown_inactivity_ms})
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
  """
  @spec get_session_id(pid()) :: Livebook.Session.id()
  def get_session_id(pid) do
    GenServer.call(pid, :get_session_id)
  end

  @doc """
  Deploys a new notebook into the app.
  """
  @spec deploy(pid(), Livebook.Notebook.t()) :: :ok
  def deploy(pid, notebook) do
    GenServer.cast(pid, {:deploy, notebook})
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
  def init({notebook, auto_shutdown_inactivity_ms}) do
    {:ok,
     %{
       version: 1,
       notebook: notebook,
       sessions: [],
       auto_shutdown_inactivity_ms: auto_shutdown_inactivity_ms,
       inactivities: %{}
     }
     |> start_eagerly()}
  end

  @impl true
  def handle_call(:describe_self, _from, state) do
    {:reply, self_from_state(state), state}
  end

  def handle_call(:get_session_id, _from, state) do
    {session_id, state} =
      case {state.notebook.app_settings.multi_session, single_session_app_session(state)} do
        {false, %{} = app_session} ->
          {app_session.id, state}

        _ ->
          {:ok, app_session} = start_app_session(state)

          state =
            state
            |> add_app_session(app_session)
            |> notify_update()

          {app_session.id, state}
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
  def handle_cast({:deploy, notebook}, state) do
    true = notebook.app_settings.slug == state.notebook.app_settings.slug

    {:noreply,
     %{state | notebook: notebook, version: state.version + 1}
     |> start_eagerly()
     |> reschedule_shutdowns()
     |> shutdown_old_versions()
     |> notify_update()}
  end

  @impl true
  def handle_info({:app_status_changed, session_id, status}, state) do
    state = update_app_session(state, session_id, &%{&1 | app_status: status})
    {:noreply, state |> shutdown_old_versions() |> notify_update()}
  end

  def handle_info({:app_client_count_changed, session_id, client_count}, state) do
    any_clients? = client_count > 0

    state =
      if any_clients? do
        untrack_inactivity(state, session_id)
      else
        track_inactivity(state, session_id)
      end

    state = update_app_session(state, session_id, &%{&1 | client_count: client_count})

    {:noreply, notify_update(state)}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    app_session = Enum.find(state.sessions, &(&1.pid == pid))
    state = update_in(state.sessions, &(&1 -- [app_session]))
    state = untrack_inactivity(state, app_session.id)
    {:noreply, notify_update(state)}
  end

  def handle_info({:inactive_timeout, session_id}, state) do
    app_session = Enum.find(state.sessions, &(&1.id == session_id))
    shutdown_session(app_session)
    {:noreply, state}
  end

  defp self_from_state(state) do
    %{
      slug: state.notebook.app_settings.slug,
      pid: self(),
      version: state.version,
      notebook_name: state.notebook.name,
      public?: state.notebook.app_settings.access_type == :public,
      sessions: state.sessions
    }
  end

  defp single_session_app_session(state) do
    app_session = Enum.find(state.sessions, &(&1.version == state.version))

    if app_session do
      if state.notebook.app_settings.zero_downtime and app_session.app_status != :executed do
        Enum.find(state.sessions, &(&1.app_status == :executed))
      end || app_session
    end
  end

  defp start_eagerly(state) when state.notebook.app_settings.multi_session, do: state

  defp start_eagerly(state) do
    if temporary_sessions?(state.notebook.app_settings) do
      state
    else
      {:ok, app_session} = start_app_session(state)
      add_app_session(state, app_session)
    end
  end

  defp start_app_session(state) do
    opts = [notebook: state.notebook, mode: :app, app_pid: self()]

    case Livebook.Sessions.create_session(opts) do
      {:ok, session} ->
        app_session = %{
          id: session.id,
          pid: session.pid,
          version: state.version,
          created_at: session.created_at,
          app_status: :executing,
          client_count: 0
        }

        Process.monitor(session.pid)

        {:ok, app_session}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp add_app_session(state, app_session) do
    state = update_in(state.sessions, &[app_session | &1])
    track_inactivity(state, app_session.id)
  end

  defp update_app_session(state, session_id, fun) do
    update_in(state.sessions, fn sessions ->
      Enum.map(sessions, fn
        %{id: ^session_id} = session -> fun.(session)
        session -> session
      end)
    end)
  end

  defp track_inactivity(state, session_id) do
    timer_ref =
      if timer_ms = shutdown_after_ms(state) do
        Process.send_after(self(), {:inactive_timeout, session_id}, timer_ms)
      end

    put_in(state.inactivities[session_id], %{since: DateTime.utc_now(), timer_ref: timer_ref})
  end

  defp untrack_inactivity(state, session_id) do
    {inactivity, state} = pop_in(state.inactivities[session_id])

    if timer_ref = inactivity[:timer_ref] do
      Process.cancel_timer(timer_ref)
    end

    state
  end

  defp shutdown_after_ms(state) do
    state.auto_shutdown_inactivity_ms[state.notebook.app_settings.auto_shutdown_type]
  end

  defp temporary_sessions?(app_settings) do
    app_settings.auto_shutdown_type in [:inactive_5s, :inactive_1m, :inactive_1h]
  end

  defp reschedule_shutdowns(state) do
    now = DateTime.utc_now()

    timer_ms = shutdown_after_ms(state)

    inactivities =
      Map.new(state.inactivities, fn {session_id, inactivity} ->
        if timer_ref = inactivity.timer_ref do
          Process.cancel_timer(timer_ref)
        end

        timer_ref =
          if timer_ms do
            inactivity_ms = DateTime.diff(now, inactivity.since, :millisecond)
            adjusted_timer_ms = max(timer_ms - inactivity_ms, 0)
            Process.send_after(self(), {:inactive_timeout, session_id}, adjusted_timer_ms)
          end

        inactivity = put_in(inactivity.timer_ref, timer_ref)

        {session_id, inactivity}
      end)

    %{state | inactivities: inactivities}
  end

  defp shutdown_old_versions(state)
       when state.notebook.app_settings.auto_shutdown_type == :new_version do
    single_session_app_session =
      unless state.notebook.app_settings.multi_session do
        single_session_app_session(state)
      end

    for app_session <- state.sessions,
        app_session != single_session_app_session,
        app_session.version < state.version do
      shutdown_session(app_session)
    end

    state
  end

  defp shutdown_old_versions(state), do: state

  defp shutdown_session(app_session) do
    if Livebook.Session.Data.app_active?(app_session.app_status) do
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
