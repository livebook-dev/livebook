defmodule Livebook.Apps do
  # Top-level module for keeping track of apps.
  #
  # App processes are tracked using `Livebook.Tracker` in the same way
  # that sessions are.

  require Logger

  alias Livebook.App

  @doc """
  Returns app process pid for the given slug.
  """
  @spec fetch_pid(App.slug()) :: {:ok, pid()} | :error
  def fetch_pid(slug) do
    case :global.whereis_name(global_name(slug)) do
      :undefined -> :error
      pid -> {:ok, pid}
    end
  end

  @doc """
  Returns app info for the given slug.
  """
  @spec fetch_app(App.slug()) :: {:ok, App.t()} | :error
  def fetch_app(slug) do
    with {:ok, pid} <- fetch_pid(slug) do
      {:ok, App.get_by_pid(pid)}
    end
  end

  @doc """
  Checks if app with the given slug exists.
  """
  @spec exists?(App.slug()) :: boolean()
  def exists?(slug) do
    :global.whereis_name(global_name(slug)) != :undefined
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

  @doc """
  Returns a global registration name for the given app slug.
  """
  @spec global_name(App.slug()) :: term()
  def global_name(slug), do: {:app, slug}

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
  Subscribes to updates in the apps list.

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

  @doc """
  Gets all permanent app specs.

  Permanent apps are apps that should be kept running in the cluster.
  This includes apps from local directory and apps from hubs.
  """
  @spec get_permanent_app_specs() :: list(Livebook.Apps.AppSpec.t())
  def get_permanent_app_specs() do
    app_specs = get_startup_app_specs() ++ Livebook.Hubs.get_app_specs()

    # Just in case there is a slug conflict between startup specs and
    # hub specs, we ensure slug uniqueness
    Enum.uniq_by(app_specs, & &1.slug)
  end

  @startup_app_specs_key :livebook_startup_app_specs

  @doc """
  Sets permanent app specs that are kept only in memory.
  """
  @spec set_startup_app_specs(list(Livebook.Apps.AppSpec.t())) :: :ok
  def set_startup_app_specs(app_specs) do
    :persistent_term.put(@startup_app_specs_key, app_specs)
  end

  @doc """
  Gets the startup permanent app specs.
  """
  @spec get_startup_app_specs() :: list(Livebook.Apps.AppSpec.t())
  def get_startup_app_specs() do
    :persistent_term.get(@startup_app_specs_key, [])
  end

  @doc """
  Builds app specs for notebooks in the given directory.

  ## Options

    * `:password` - a password to set for every loaded app

    * `:should_warmup` - whether the app should be warmed up before
      deployment. Disabling warmup makes sense if the app setup has
      already been cached. Defaults to `true`

    * `:hub_id` - when set, only imports notebooks from the given hub,
      other notebooks are ignored

  """
  @spec build_app_specs_in_dir(String.t(), keyword()) :: list(Livebook.Apps.AppSpec.t())
  def build_app_specs_in_dir(path, opts \\ []) do
    opts = Keyword.validate!(opts, [:password, :hub_id, should_warmup: true])

    infos = import_app_notebooks(path, opts[:hub_id])

    if infos == [] do
      Logger.warning("No .livemd files were found for deployment at #{path}")
    end

    for %{status: {:error, message}} = info <- infos do
      Logger.warning(
        "Ignoring app at #{info.relative_path}. #{Livebook.Utils.upcase_first(message)}."
      )
    end

    infos = Enum.filter(infos, &(&1.status == :ok))

    infos =
      infos
      |> Enum.reduce({[], %{}}, fn info, {infos, slugs} ->
        slug = info.notebook.app_settings.slug

        case slugs do
          %{^slug => other_path} ->
            Logger.warning(
              "Ignoring app at #{info.relative_path}. App with the same slug (#{slug}) is already present at #{other_path}"
            )

            {infos, slugs}

          %{} ->
            {[info | infos], Map.put(slugs, slug, info.relative_path)}
        end
      end)
      |> elem(0)
      |> Enum.reverse()

    for info <- infos, info.import_warnings != [] do
      items = Enum.map(info.import_warnings, &("- " <> &1))

      Logger.warning(
        "Found warnings while importing app notebook at #{info.relative_path}:\n  " <>
          Enum.join(items, "\n  ")
      )
    end

    for %{notebook: notebook} = info <- infos do
      if opts[:password] == nil and notebook.app_settings.access_type == :protected do
        Logger.warning(
          "The app at #{info.relative_path} will use a random password." <>
            " You may want to set LIVEBOOK_APPS_PATH_PASSWORD or make the app public."
        )
      end

      %Livebook.Apps.PathAppSpec{
        slug: notebook.app_settings.slug,
        path: info.path,
        password: opts[:password],
        should_warmup: opts[:should_warmup]
      }
    end
  end

  defp import_app_notebooks(dir, hub_id) do
    pattern = Path.join([dir, "**", "*.livemd"])

    for path <- Path.wildcard(pattern) do
      markdown = File.read!(path)

      {notebook, %{warnings: warnings, stamp_verified?: stamp_verified?}} =
        Livebook.LiveMarkdown.notebook_from_livemd(markdown)

      status =
        cond do
          not Livebook.Notebook.AppSettings.valid?(notebook.app_settings) ->
            {:error,
             "the deployment settings are missing or invalid. Please configure them under the notebook deploy panel"}

          # We only import notebooks from the given hub, but if that
          # option is set then there should really be no other ones,
          # so it makes sense to warn if there are
          hub_id && notebook.hub_id != hub_id ->
            {:error, "the notebook does not come from hub #{hub_id}"}

          # We only deploy apps with valid stamp. We make an exception
          # for personal hub, because the deployment instance has a
          # different personal secret key anyway
          notebook.hub_id != Livebook.Hubs.Personal.id() and not stamp_verified? ->
            {:error, "the notebook does not have a valid stamp"}

          true ->
            :ok
        end

      notebook_file = Livebook.FileSystem.File.local(path)
      files_dir = Livebook.Session.files_dir_for_notebook(notebook_file)

      %{
        path: path,
        relative_path: Path.relative_to(path, dir),
        status: status,
        notebook: notebook,
        import_warnings: warnings,
        files_source: {:dir, files_dir}
      }
    end
  end

  @doc """
  Returns a temporary dir for app files.
  """
  @spec generate_files_tmp_path(String.t()) :: String.t()
  def generate_files_tmp_path(slug) do
    Path.join([
      Livebook.Config.tmp_path(),
      "app_files",
      slug <> Livebook.Utils.random_short_id()
    ])
  end

  @doc """
  Runs app warmup.

  This evaluates the setup cell in the given notebook to populate the
  relevant caches, such as dependency installation.
  """
  @spec warmup_app(Livebook.Notebook.t(), String.t()) :: :ok | {:error, String.t()}
  def warmup_app(notebook, files_tmp_path) do
    run_app_setup_sync(notebook, files_tmp_path)
  end

  @doc """
  Same as `warmup_app/2`, but loads app spec and immediately cleans up
  afterwards.
  """
  @spec warmup_app(Livebook.Apps.AppSpec.t()) :: :ok | {:error, String.t()}
  def warmup_app(app_spec) do
    files_tmp_path = generate_files_tmp_path(app_spec.slug)

    result =
      with {:ok, %{notebook: notebook}} <- Livebook.Apps.AppSpec.load(app_spec, files_tmp_path) do
        warmup_app(notebook, files_tmp_path)
      end

    File.rm_rf(files_tmp_path)

    result
  end

  defp run_app_setup_sync(notebook, files_tmp_path) do
    files_source =
      {:dir,
       files_tmp_path
       |> Livebook.FileSystem.Utils.ensure_dir_path()
       |> Livebook.FileSystem.File.local()}

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
end
