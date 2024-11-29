defmodule Livebook.App do
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
    :sessions,
    :app_spec,
    :permanent
  ]

  use GenServer, restart: :temporary

  require Logger

  @type t :: %{
          slug: slug(),
          pid: pid(),
          version: pos_integer(),
          warnings: list(String.t()),
          notebook_name: String.t(),
          public?: boolean(),
          multi_session: boolean(),
          sessions: list(app_session()),
          app_spec: Livebook.Apps.AppSpec.t(),
          permanent: boolean()
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

  @typedoc """
  Notebook and related information for deploying an app version.

  This information is used to start app sessions.

    * `:notebook` - the notebook to use for the deployment

    * `:files_tmp_path` - a path to local directory with notebook files.
      This should be a temporary copy of the files that the app process
      takes ownership over. The app is going to remove the directory
      once no longer needed.

      The app uses this local copy of the files, so that changes to
      the original files can be made safely. Also, if the original
      files are stored on a remote file system, we want to download
      them once, rather than in each starting session. Finally, some
      app specs (such as Teams) may need to unpack their files into
      a temporary directory and only the app process knows when to
      remove this directory

    * `:app_spec` - the app spec that was used to load the notebook

    * `:permanent` - whether the app is being deployed as a permanent,
      typically by `Livebook.Apps.Manager`

    * `:warnings` - a list of warnings to show for the deployment

  """
  @type deployment_bundle :: %{
          notebook: Livebook.Notebook.t(),
          files_tmp_path: String.t(),
          app_spec: Livebook.Apps.AppSpec.t(),
          permanent: boolean(),
          warnings: list(String.t()),
          deployed_by: Livebook.Users.User.t() | nil
        }

  @doc """
  Starts an apps process.

  ## Options

    * `:deployment_bundle` (required) - see `t:deployment_bundle/0`

  """
  @spec start_link(keyword()) :: {:ok, pid} | {:error, any()}
  def start_link(opts) do
    opts = Keyword.validate!(opts, [:deployment_bundle])
    deployment_bundle = Keyword.fetch!(opts, :deployment_bundle)

    GenServer.start_link(__MODULE__, deployment_bundle)
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
  @spec deploy(pid(), deployment_bundle()) :: :ok
  def deploy(pid, deployment_bundle) do
    GenServer.cast(pid, {:deploy, deployment_bundle})
  end

  @doc """
  Closes the app.

  This operation results in all app sessions being closed as well.
  """
  @spec close(pid()) :: :ok
  def close(pid) do
    GenServer.call(pid, :close)
  end

  @doc """
  Sends an asynchronous request to close the session.
  """
  @spec close_async(pid()) :: :ok
  def close_async(pid) do
    GenServer.cast(pid, :close)
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
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "apps:#{slug}")
  end

  @impl true
  def init(deployment_bundle) do
    state = %{
      version: 1,
      deployment_bundle: deployment_bundle,
      sessions: [],
      users: %{}
    }

    app = self_from_state(state)

    slug = deployment_bundle.app_spec.slug
    name = Livebook.Apps.global_name(slug)

    case :global.register_name(name, self(), &resolve_app_conflict/3) do
      :yes ->
        with :ok <- Livebook.Tracker.track_app(app) do
          {:ok, state, {:continue, :after_init}}
        end

      :no ->
        {:error, :already_started}
    end
  end

  @impl true
  def handle_continue(:after_init, state) do
    {:noreply, state |> start_eagerly() |> notify_update()}
  end

  @impl true
  def handle_call(:describe_self, _from, state) do
    {:reply, self_from_state(state), state}
  end

  def handle_call({:get_session_id, user}, _from, state) do
    {session_id, state} =
      case {state.deployment_bundle.notebook.app_settings.multi_session,
            single_session_app_session(state)} do
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
    {:reply, state.deployment_bundle.notebook.app_settings, state}
  end

  def handle_call(:close, _from, state) do
    {:stop, :shutdown, :ok, state}
  end

  @impl true
  def handle_cast({:deploy, deployment_bundle}, state) do
    assert_valid_redeploy!(state.deployment_bundle, deployment_bundle)

    cleanup_notebook_files_dir(state)

    {:noreply,
     %{state | version: state.version + 1, deployment_bundle: deployment_bundle}
     |> start_eagerly()
     |> shutdown_old_versions()
     |> notify_update()}
  end

  def handle_cast(:close, state) do
    {:stop, :shutdown, state}
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

  @impl true
  def terminate(_reason, state) do
    cleanup_notebook_files_dir(state)
    :ok
  end

  defp self_from_state(state) do
    %{
      slug: state.deployment_bundle.notebook.app_settings.slug,
      pid: self(),
      version: state.version,
      warnings: state.deployment_bundle.warnings,
      notebook_name: state.deployment_bundle.notebook.name,
      public?: state.deployment_bundle.notebook.app_settings.access_type == :public,
      multi_session: state.deployment_bundle.notebook.app_settings.multi_session,
      sessions: state.sessions,
      app_spec: state.deployment_bundle.app_spec,
      permanent: state.deployment_bundle.permanent
    }
  end

  defp resolve_app_conflict({:app, slug}, pid1, pid2) do
    Logger.info("[app=#{slug}] Closing duplicate app in the cluster")
    [keep_pid, close_pid] = Enum.shuffle([pid1, pid2])
    close_async(close_pid)
    keep_pid
  end

  defp single_session_app_session(state) do
    app_session = Enum.find(state.sessions, &(&1.version == state.version))

    if app_session do
      if state.deployment_bundle.notebook.app_settings.zero_downtime and
           not status_ready?(app_session.app_status) do
        Enum.find(state.sessions, &status_ready?(&1.app_status))
      end || app_session
    end
  end

  defp status_ready?(%{execution: :executed, lifecycle: :active}), do: true
  defp status_ready?(_status), do: false

  defp start_eagerly(state) when state.deployment_bundle.notebook.app_settings.multi_session,
    do: state

  defp start_eagerly(state) do
    if temporary_sessions?(state.deployment_bundle.notebook.app_settings) do
      state
    else
      {:ok, state, _app_session} = start_app_session(state)
      state
    end
  end

  defp start_app_session(state, user \\ nil) do
    user = if(state.deployment_bundle.notebook.teams_enabled, do: user)

    files_source =
      state.deployment_bundle.files_tmp_path
      |> Livebook.FileSystem.Utils.ensure_dir_path()
      |> Livebook.FileSystem.File.local()

    opts = [
      notebook: state.deployment_bundle.notebook,
      files_source: {:dir, files_source},
      mode: :app,
      app_pid: self(),
      auto_shutdown_ms: state.deployment_bundle.notebook.app_settings.auto_shutdown_ms,
      started_by: user,
      deployed_by: state.deployment_bundle.deployed_by
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

  defp shutdown_old_versions(state)
       when not state.deployment_bundle.notebook.app_settings.multi_session do
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
    broadcast_message(state.deployment_bundle.notebook.app_settings.slug, {:app_updated, app})
    state
  end

  defp broadcast_message(slug, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "apps:#{slug}", message)
  end

  defp cleanup_notebook_files_dir(state) do
    if path = state.deployment_bundle.files_tmp_path do
      File.rm_rf(path)
    end
  end

  defp assert_valid_redeploy!(
         %{app_spec: %module{slug: slug}, permanent: permanent},
         %{app_spec: %module{slug: slug}, permanent: permanent}
       ),
       do: :ok
end
