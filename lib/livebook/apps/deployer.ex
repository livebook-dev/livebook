defmodule Livebook.Apps.Deployer do
  # Deploys apps on the given node.
  #
  # Each node runs a single deployer, which deploys apps sequentially.
  # This design makes sure that only one app warmup is going to run
  # at a time, rather than concurrently, which reduces memory usage
  # and prevents race conditions with `Mix.install/2`.

  use GenServer

  require Logger

  alias Livebook.App
  alias Livebook.Apps

  @doc """
  Starts a new deployer process.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {})
  end

  @doc """
  Asynchronously requests an app deployment.

  If there is no app process under the requested slug, it is started.
  Otherwise the notebook is deployed as a new version into the existing
  app.

  This function automatically starts monitoring the deployer process
  and returns the monitor reference.

  When the deployment finished the caller receives a message of the
  form `{:deploy_result, ref, result}`, with the same monitor reference
  and the result of `Apps.deploy/2`. The caller should then
  demonitor the reference.

  ## Options

    * `:start_only` - when `true`, deploys only if the app does not
      exist already. If the app does exist, the deployment finishes
      with an error. Defaults to `false`

    * `:permanent` - whether the app is deployed as permanent. Defaults
      to `false`

    * `:deployed_by` - the user performing the deployment

  """
  @spec deploy_monitor(pid(), Apps.AppSpec.t(), keyword()) :: reference()
  def deploy_monitor(pid, app_spec, opts \\ []) do
    opts = Keyword.validate!(opts, start_only: false, permanent: false, deployed_by: nil)

    ref = Process.monitor(pid)

    GenServer.cast(
      pid,
      {:deploy, app_spec, opts[:start_only], opts[:permanent], opts[:deployed_by], self(), ref}
    )

    ref
  end

  @doc """
  Returns a list of all deployers present in the cluster.
  """
  @spec list_deployers() :: list(pid())
  def list_deployers() do
    :pg.get_members(Apps.Deployer.PG, Apps.Deployer)
  end

  @doc """
  Returns a node-local deployer.
  """
  @spec local_deployer(node()) :: pid()
  def local_deployer(node \\ node()) do
    list_deployers() |> Enum.find(&(node(&1) == node)) || raise "no local deployer running"
  end

  @impl true
  def init({}) do
    :pg.join(Apps.Deployer.PG, __MODULE__, self())

    {:ok, %{}}
  end

  @impl true
  def handle_cast({:deploy, app_spec, start_only, permanent, deployed_by, from, ref}, state) do
    Logger.info("[app=#{app_spec.slug}] Deploying app")

    files_tmp_path = Apps.generate_files_tmp_path(app_spec.slug)

    result =
      with {:ok, %{notebook: notebook, warnings: warnings}} <-
             Apps.AppSpec.load(app_spec, files_tmp_path) do
        if Apps.AppSpec.should_warmup?(app_spec) do
          with {:error, message} <- Apps.warmup_app(notebook, files_tmp_path) do
            Logger.warning("[app=#{app_spec.slug}] App warmup failed, #{message}")
          end
        end

        deployment_bundle = %{
          notebook: notebook,
          files_tmp_path: files_tmp_path,
          app_spec: app_spec,
          permanent: permanent,
          warnings: warnings,
          deployed_by: deployed_by
        }

        name = Apps.global_name(app_spec.slug)
        opts = [start_only: start_only]

        with {:error, error} <- start_or_redeploy(name, deployment_bundle, opts) do
          File.rm_rf(files_tmp_path)
          {:error, error}
        end
      end

    send(from, {:deploy_result, ref, result})

    {:noreply, state}
  end

  defp start_or_redeploy(name, deployment_bundle, opts) do
    case :global.whereis_name(name) do
      :undefined ->
        opts = [deployment_bundle: deployment_bundle]

        case DynamicSupervisor.start_child(Livebook.AppSupervisor, {App, opts}) do
          {:ok, pid} ->
            {:ok, pid}

          {:error, :already_started} ->
            # We could use a global transaction to prevent the unlikely
            # case of multiple nodes starting the app simultaneously.
            # However, the global registration can still fail if the
            # node joins the cluster while the app process is starting.
            # So we handle both cases the same way in this branch.
            start_or_redeploy(name, deployment_bundle, opts)

          {:error, reason} ->
            {:error, "app process failed to start, reason: #{inspect(reason)}"}
        end

      pid ->
        redeploy_app(pid, deployment_bundle, opts)
    end
  end

  defp redeploy_app(pid, deployment_bundle, opts) do
    cond do
      opts[:start_only] ->
        {:error, :already_started}

      node(pid) != node() ->
        # This is relevant specifically when :files_source points to
        # a local directory, but we generally expect redeployments to
        # happen within the same node, so it's good to enforce this
        {:error, "cannot deploy a new notebook to an app running on a different node"}

      true ->
        App.deploy(pid, deployment_bundle)
        {:ok, pid}
    end
  end
end
