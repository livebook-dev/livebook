defmodule Livebook.Apps do
  @moduledoc false

  require Logger

  alias Livebook.Session

  @doc """
  Registers an app session under the given slug.

  In case another app is already registered under the given slug,
  this function atomically replaces the registration and instructs
  the previous app to shut down.
  """
  @spec register(pid(), String.t()) :: :ok
  def register(session_pid, slug) do
    name = name(slug)

    :global.trans({{:app_registration, name}, node()}, fn ->
      case :global.whereis_name(name) do
        :undefined ->
          :ok

        pid ->
          :global.unregister_name(name)
          Session.app_unregistered(pid)
      end

      :yes = :global.register_name(name, session_pid)
    end)

    :ok
  end

  @doc """
  Unregisters an app session from the given slug.
  """
  @spec unregister(pid(), String.t()) :: :ok
  def unregister(session_pid, slug) do
    name = name(slug)

    :global.trans({{:app_registration, name}, node()}, fn ->
      case :global.whereis_name(name) do
        :undefined ->
          :ok

        ^session_pid ->
          :global.unregister_name(name)
      end
    end)

    :ok
  end

  @doc """
  Checks if app with the given slug exists.
  """
  @spec exists?(String.t()) :: boolean()
  def exists?(slug) do
    :global.whereis_name(name(slug)) != :undefined
  end

  @doc """
  Looks up app session with the given slug.
  """
  @spec fetch_session_by_slug(String.t()) :: {:ok, Session.t()} | :error
  def fetch_session_by_slug(slug) do
    case :global.whereis_name(name(slug)) do
      :undefined ->
        :error

      pid ->
        session = Session.get_by_pid(pid)
        {:ok, session}
    end
  end

  @doc """
  Looks up app session with the given slug and returns its settings.
  """
  @spec fetch_settings_by_slug(String.t()) :: {:ok, Livebook.Notebook.AppSettings.t()} | :error
  def fetch_settings_by_slug(slug) do
    case :global.whereis_name(name(slug)) do
      :undefined ->
        :error

      pid ->
        app_settings = Session.get_app_settings(pid)
        {:ok, app_settings}
    end
  end

  defp name(slug), do: {:app, slug}

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

    for path <- paths do
      markdown = File.read!(path)
      {notebook, warnings} = Livebook.LiveMarkdown.notebook_from_livemd(markdown)

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
        {:ok, session} = Livebook.Sessions.create_session(notebook: notebook, mode: :app)
        Livebook.Session.app_build(session.pid)
      else
        Logger.warning("Skipping app deployment at #{path} due to invalid settings")
      end
    end

    :ok
  end
end
