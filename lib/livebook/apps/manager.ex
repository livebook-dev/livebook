defmodule Livebook.Apps.Manager do
  # Orchestrates permanent app deployments across the cluster.
  #
  # Only a single instance of the manager runs in the cluster. This is
  # ensured by registering it in :global. Each node also runs a single
  # instance of `Livebook.Apps.ManagerWatcher`, which starts a new
  # manager whenever needed.

  use GenServer, restart: :temporary

  require Logger

  alias Livebook.Apps

  @name __MODULE__

  config = Application.compile_env(:livebook, __MODULE__)

  @handle_app_close_debounce_ms 100
  @retry_backoff_base_ms Keyword.fetch!(config, :retry_backoff_base_ms)

  @doc """
  Starts a new manager process.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {}, name: {:global, @name})
  end

  @doc """
  Checks if the manager is running on the current node.
  """
  @spec local?() :: boolean()
  def local?() do
    case :global.whereis_name(@name) do
      :undefined -> false
      pid -> node(pid) == node()
    end
  end

  @doc """
  Asynchronously asks the manager to reflect the currently configured
  permanent apps.

  Any permanent apps that are not running will be deployed. Apps that
  should no longer be running will be closed.
  """
  @spec sync_permanent_apps() :: :ok
  def sync_permanent_apps() do
    GenServer.cast({:global, @name}, :sync_permanent_apps)
  end

  @impl true
  def init({}) do
    Apps.subscribe()

    state = %{deployments: %{}, handle_app_close_timer_ref: nil}

    {:ok, state, {:continue, :after_init}}
  end

  @impl true
  def handle_continue(:after_init, state) do
    {:noreply, sync_apps(state)}
  end

  @impl true
  def handle_cast(:sync_permanent_apps, state) do
    {:noreply, sync_apps(state)}
  end

  @impl true
  def handle_info({:app_closed, _app}, state) do
    # When a new node joins the cluster it has a node-local instance
    # of the manager and it may have deployed apps. :global may resolve
    # app conflicts before the manager conflicts (so both managers keep
    # running). The app conflict resolution involves closing one of the
    # running apps, which would cause the app node-local tracker emit
    # :app_closed to that node-local manager immediately. At that point
    # the given app may not be registered in :global temporarily, which
    # would make us redeploy it unnecessarily.
    #
    # To avoid race conditions like that, we debounce the redeployment
    # to give more time for conflicts to be resolved and the system to
    # settle down.

    if ref = state.handle_app_close_timer_ref do
      Process.cancel_timer(ref)
    end

    handle_app_close_timer_ref =
      Process.send_after(self(), :handle_app_close, @handle_app_close_debounce_ms)

    {:noreply, %{state | handle_app_close_timer_ref: handle_app_close_timer_ref}}
  end

  def handle_info(:handle_app_close, state) do
    state = %{state | handle_app_close_timer_ref: nil}
    {:noreply, sync_apps(state)}
  end

  def handle_info({:DOWN, ref, :process, _pid, reason}, state) do
    deployment = deployment_by_ref(state, ref)
    message = "deployer terminated unexpectedly, reason: #{Exception.format_exit(reason)}"
    {:noreply, handle_deployment_failure(state, deployment, message)}
  end

  def handle_info({:deploy_result, ref, result}, state) do
    Process.demonitor(ref, [:flush])

    deployment = deployment_by_ref(state, ref)

    case result do
      {:ok, _pid} ->
        {_, state} = pop_in(state.deployments[deployment.slug])
        Logger.info("[app=#{deployment.slug}] Deployment successful, app running")
        # Run sync in case there is already a new version for this app
        {:noreply, sync_apps(state)}

      {:error, error} ->
        {:noreply, handle_deployment_failure(state, deployment, error)}
    end
  end

  def handle_info({:retry, slug}, state) do
    {:noreply, retry(state, slug)}
  end

  def handle_info(_message, state) do
    {:noreply, state}
  end

  defp sync_apps(state) do
    permanent_app_specs = Apps.get_permanent_app_specs()
    state = deploy_missing_apps(state, permanent_app_specs)
    close_leftover_apps(permanent_app_specs)
    state
  end

  defp deploy_missing_apps(state, permanent_app_specs) do
    for app_spec <- permanent_app_specs,
        not Map.has_key?(state.deployments, app_spec.slug),
        reduce: state do
      state ->
        case fetch_app(app_spec.slug) do
          {:ok, _state, app} when app.app_spec.version == app_spec.version ->
            state

          {:ok, :reachable, app} ->
            ref = redeploy(app, app_spec)
            track_deployment(state, app_spec, ref)

          {:ok, :unreachable, _app} ->
            state

          :error ->
            ref = deploy(app_spec)
            track_deployment(state, app_spec, ref)
        end
    end
  end

  defp close_leftover_apps(permanent_app_specs) do
    permanent_slugs = MapSet.new(permanent_app_specs, & &1.slug)

    for app <- Apps.list_apps(),
        app.permanent,
        app.slug not in permanent_slugs do
      Livebook.App.close_async(app.pid)
    end
  end

  defp fetch_app(slug) do
    # We check both global and the tracker. The app may be present in
    # the tracker for longer, but if it is actually down we will get
    # the :app_closed event eventually. On the other hand, if the app
    # is already somewhere, global will tell us sooner than tracker.
    case Apps.fetch_app(slug) do
      {:ok, app} ->
        {:ok, :reachable, app}

      :error ->
        case Livebook.Tracker.fetch_app(slug) do
          {:ok, app} -> {:ok, :unreachable, app}
          :error -> :error
        end
    end
  end

  defp app_definitely_down?(slug) do
    not Apps.exists?(slug) and Livebook.Tracker.fetch_app(slug) == :error
  end

  defp deploy(app_spec) do
    deployer_pid = Livebook.Apps.Deployer.list_deployers() |> Enum.random()
    Logger.info("[app=#{app_spec.slug}] Scheduling app deployment on node #{node(deployer_pid)}")

    Livebook.Apps.Deployer.deploy_monitor(deployer_pid, app_spec,
      permanent: true,
      start_only: true
    )
  end

  defp redeploy(app, app_spec) do
    # Redeploying pushes the new notebook to an existing app process,
    # so we need to run deployment on the same node
    node = node(app.pid)
    deployer_pid = Livebook.Apps.Deployer.local_deployer(node)
    Logger.info("[app=#{app_spec.slug}] Scheduling app deployment on node #{node(deployer_pid)}")
    Livebook.Apps.Deployer.deploy_monitor(deployer_pid, app_spec, permanent: true)
  end

  defp track_deployment(state, app_spec, ref) do
    put_in(state.deployments[app_spec.slug], %{
      ref: ref,
      retries: 0,
      app_spec: app_spec,
      slug: app_spec.slug
    })
  end

  defp handle_deployment_failure(state, deployment, message) do
    Logger.error("[app=#{deployment.slug}] Deployment failed, #{message}")

    # Schedule retry
    %{app_spec: app_spec, retries: retries} = deployment
    retries = retries + 1
    time = @retry_backoff_base_ms * min(retries, 6)
    Process.send_after(self(), {:retry, app_spec.slug}, time)
    put_in(state.deployments[app_spec.slug].retries, retries)
  end

  defp retry(state, slug) do
    if app_definitely_down?(slug) do
      %{app_spec: app_spec} = state.deployments[slug]
      ref = deploy(app_spec)
      put_in(state.deployments[slug].ref, ref)
    else
      {_, state} = pop_in(state.deployments[slug])
      state
    end
  end

  defp deployment_by_ref(state, ref) do
    Enum.find_value(state.deployments, fn {_slug, deployment} ->
      deployment.ref == ref && deployment
    end)
  end
end
