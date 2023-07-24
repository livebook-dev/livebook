defmodule Livebook.Apps do
  @moduledoc false

  # This module is responsible for starting and discovering apps.
  #
  # App processes are tracked using `Livebook.Tracker` in the same way
  # that sessions are.

  require Logger

  alias Livebook.App

  @doc """
  Deploys the given notebook as an app.

  If there is no app process under the corresponding slug, it is started.
  Otherwise the notebook is deployed as a new version into the existing
  app.

  ## Options

    * `:warnings` - a list of warnings to show for the new deployment

    * `:files_source` - a location to fetch notebook files from, see
      `Livebook.Session.start_link/1` for more details

  """
  @spec deploy(Livebook.Notebook.t(), keyword()) :: {:ok, pid()} | {:error, term()}
  def deploy(notebook, opts \\ []) do
    opts = Keyword.validate!(opts, warnings: [], files_source: nil)

    slug = notebook.app_settings.slug
    name = name(slug)

    case :global.whereis_name(name) do
      :undefined ->
        :global.trans({{:app_registration, name}, node()}, fn ->
          case :global.whereis_name(name) do
            :undefined ->
              with {:ok, pid} <- start_app(notebook, opts[:warnings], opts[:files_source]) do
                :yes = :global.register_name(name, pid)
                {:ok, pid}
              end

            pid ->
              App.deploy(pid, notebook,
                warnings: opts[:warnings],
                files_source: opts[:files_source]
              )

              {:ok, pid}
          end
        end)

      pid ->
        App.deploy(pid, notebook, warnings: opts[:warnings], files_source: opts[:files_source])
        {:ok, pid}
    end
  end

  defp start_app(notebook, warnings, files_source) do
    opts = [notebook: notebook, warnings: warnings, files_source: files_source]

    case DynamicSupervisor.start_child(Livebook.AppSupervisor, {App, opts}) do
      {:ok, pid} ->
        app = App.get_by_pid(pid)

        case Livebook.Tracker.track_app(app) do
          :ok ->
            {:ok, pid}

          {:error, reason} ->
            App.close(pid)
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Returns app process pid for the given slug.
  """
  @spec fetch_pid(App.slug()) :: {:ok, pid()} | :error
  def fetch_pid(slug) do
    case :global.whereis_name(name(slug)) do
      :undefined -> :error
      pid -> {:ok, pid}
    end
  end

  @doc """
  Returns app info for the given slug.
  """
  @spec fetch_app(App.slug()) :: {:ok, App.t()} | :error
  def fetch_app(slug) do
    case :global.whereis_name(name(slug)) do
      :undefined -> :error
      pid -> {:ok, App.get_by_pid(pid)}
    end
  end

  @doc """
  Checks if app with the given slug exists.
  """
  @spec exists?(App.slug()) :: boolean()
  def exists?(slug) do
    :global.whereis_name(name(slug)) != :undefined
  end

  @doc """
  Looks up app with the given slug and returns its settings.
  """
  @spec fetch_settings(App.slug()) :: {:ok, Livebook.Notebook.AppSettings.t()} | :error
  def fetch_settings(slug) do
    with {:ok, pid} <- fetch_pid(slug) do
      app_settings = App.get_settings(pid)
      {:ok, app_settings}
    end
  end

  defp name(slug), do: {:app, slug}

  @doc """
  Returns all the running apps.
  """
  @spec list_apps() :: list(App.t())
  def list_apps() do
    Livebook.Tracker.list_apps()
  end

  @doc """
  Updates the given app info across the cluster.
  """
  @spec update_app(App.t()) :: :ok | {:error, any()}
  def update_app(app) do
    Livebook.Tracker.update_app(app)
  end

  @doc """
  Subscribes to update in apps list.

  ## Messages

    * `{:app_created, app}`
    * `{:app_updated, app}`
    * `{:app_closed, app}`

  """
  @spec subscribe() :: :ok | {:error, term()}
  def subscribe() do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "tracker_apps")
  end

  @doc """
  Deploys an app for each notebook in the given directory.

  ## Options

    * `:password` - a password to set for every loaded app

    * `:warmup` - when `true`, run setup cell for each of the
      notebooks before the actual deployment. The setup cells are
      run one by one to avoid race conditions. Defaults to `true`

    * `:skip_deploy` - when `true`, the apps are not deployed.
      This can be used to warmup apps without deployment. Defaults
      to `false`

  """
  @spec deploy_apps_in_dir(String.t(), keyword()) :: :ok
  def deploy_apps_in_dir(path, opts \\ []) do
    opts = Keyword.validate!(opts, [:password, warmup: true, skip_deploy: false])

    infos = import_app_notebooks(path)

    if infos == [] do
      Logger.warning("No .livemd files were found for deployment at #{path}")
    end

    for %{status: {:error, message}} = info <- infos do
      Logger.warning(
        "Skipping deployment for app at #{info.relative_path}. #{Livebook.Utils.upcase_first(message)}."
      )
    end

    infos = Enum.filter(infos, &(&1.status == :ok))

    for info <- infos, info.import_warnings != [] do
      items = Enum.map(info.import_warnings, &("- " <> &1))

      Logger.warning(
        "Found warnings while importing app notebook at #{info.relative_path}:\n\n" <>
          Enum.join(items, "\n")
      )
    end

    if infos != [] and opts[:warmup] do
      Logger.info("Running app warmups")

      for info <- infos do
        with {:error, message} <- run_app_setup_sync(info.notebook, info.files_source) do
          Logger.warning(
            "Failed to run setup for app at #{info.relative_path}. #{Livebook.Utils.upcase_first(message)}."
          )
        end
      end
    end

    if infos != [] and not opts[:skip_deploy] do
      Logger.info("Deploying apps")

      for %{notebook: notebook} = info <- infos do
        notebook =
          if password = opts[:password] do
            put_in(notebook.app_settings.password, password)
          else
            notebook
          end

        warnings = Enum.map(info.import_warnings, &("Import: " <> &1))

        {:ok, _} = deploy(notebook, warnings: warnings, files_source: info.files_source)
      end
    end

    :ok
  end

  defp import_app_notebooks(dir) do
    pattern = Path.join([dir, "**", "*.livemd"])

    for path <- Path.wildcard(pattern) do
      markdown = File.read!(path)

      {notebook, warnings} = Livebook.LiveMarkdown.notebook_from_livemd(markdown)

      apps_path_hub_id = Livebook.Config.apps_path_hub_id()

      status =
        cond do
          not Livebook.Notebook.AppSettings.valid?(notebook.app_settings) ->
            {:error,
             "the deployment settings are missing or invalid. Please configure them under the notebook deploy panel"}

          apps_path_hub_id && apps_path_hub_id != notebook.hub_id ->
            {:error, "the notebook is not verified to come from hub #{apps_path_hub_id}"}

          true ->
            :ok
        end

      notebook_file = Livebook.FileSystem.File.local(path)
      files_dir = Livebook.Session.files_dir_for_notebook(notebook_file)

      %{
        relative_path: Path.relative_to(path, dir),
        status: status,
        notebook: notebook,
        import_warnings: warnings,
        files_source: {:dir, files_dir}
      }
    end
  end

  defp run_app_setup_sync(notebook, files_source) do
    notebook = %{notebook | sections: []}

    opts = [
      notebook: notebook,
      files_source: files_source,
      mode: :app,
      app_pid: self()
    ]

    case Livebook.Sessions.create_session(opts) do
      {:ok, %{id: session_id} = session} ->
        ref = Process.monitor(session.pid)

        receive do
          {:app_status_changed, ^session_id, status} ->
            Process.demonitor(ref)
            Livebook.Session.close(session.pid)

            if status.execution == :executed do
              :ok
            else
              {:error, "setup cell finished with failure"}
            end

          {:DOWN, ^ref, :process, _, reason} ->
            {:error, "session terminated unexpectedly, reason: #{inspect(reason)}"}
        end

      {:error, reason} ->
        {:error, "failed to start session, reason: #{inspect(reason)}"}
    end
  end

  @doc """
  Checks if the apps directory is configured and contains no notebooks.
  """
  @spec empty_apps_path?() :: boolean()
  def empty_apps_path?() do
    if path = Livebook.Config.apps_path() do
      pattern = Path.join([path, "**", "*.livemd"])
      Path.wildcard(pattern) == []
    else
      false
    end
  end
end
