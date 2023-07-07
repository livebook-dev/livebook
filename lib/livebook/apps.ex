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

  """
  @spec deploy(Livebook.Notebook.t(), keyword()) :: {:ok, pid()} | {:error, term()}
  def deploy(notebook, opts \\ []) do
    opts = Keyword.validate!(opts, warnings: [])

    slug = notebook.app_settings.slug
    name = name(slug)

    case :global.whereis_name(name) do
      :undefined ->
        :global.trans({{:app_registration, name}, node()}, fn ->
          case :global.whereis_name(name) do
            :undefined ->
              with {:ok, pid} <- start_app(notebook, opts[:warnings]) do
                :yes = :global.register_name(name, pid)
                {:ok, pid}
              end

            pid ->
              App.deploy(pid, notebook, warnings: opts[:warnings])
              {:ok, pid}
          end
        end)

      pid ->
        App.deploy(pid, notebook, warnings: opts[:warnings])
        {:ok, pid}
    end
  end

  defp start_app(notebook, warnings) do
    opts = [notebook: notebook, warnings: warnings]

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

  """
  @spec deploy_apps_in_dir(String.t(), keyword()) :: :ok
  def deploy_apps_in_dir(path, opts \\ []) do
    opts = Keyword.validate!(opts, [:password])

    pattern = Path.join([path, "**", "*.livemd"])
    paths = Path.wildcard(pattern)

    if paths == [] do
      Logger.warning("No .livemd files were found for deployment at #{path}")
    end

    for path <- paths do
      markdown = File.read!(path)

      {notebook, %{warnings: warnings, verified_hub_id: verified_hub_id}} =
        Livebook.LiveMarkdown.notebook_from_livemd(markdown)

      if warnings != [] do
        items = Enum.map(warnings, &("- " <> &1))

        Logger.warning(
          "Found warnings while importing app notebook at #{path}:\n\n" <> Enum.join(items, "\n")
        )
      end

      notebook =
        if password = opts[:password] do
          put_in(notebook.app_settings.password, password)
        else
          notebook
        end

      if Livebook.Notebook.AppSettings.valid?(notebook.app_settings) do
        warnings = Enum.map(warnings, &("Import: " <> &1))
        apps_path_hub_id = Livebook.Config.apps_path_hub_id()

        deploy_app(apps_path_hub_id, verified_hub_id, notebook, warnings, path)
      else
        Logger.warning(
          "Skipping app deployment at #{path}. The deployment settings are missing or invalid. Please configure them under the notebook deploy panel."
        )
      end
    end

    :ok
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

  defp deploy_app(nil, _, notebook, warnings, _), do: deploy(notebook, warnings: warnings)
  defp deploy_app(id, id, notebook, warnings, _), do: deploy(notebook, warnings: warnings)

  defp deploy_app(id, _, _, _, path) do
    Logger.warning(
      "Skipping app deployment at #{path}. The notebook is not verified to come from hub #{id}"
    )
  end
end
