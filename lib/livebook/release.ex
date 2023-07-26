defmodule Livebook.Release do
  @moduledoc false

  @doc """
  Runs the setup for all apps deployed from directory on startup.
  """
  def run_apps_setup() do
    start_app()

    if apps_path = Livebook.Config.apps_path() do
      Livebook.Apps.deploy_apps_in_dir(apps_path, dry_run: true)
    end
  end

  defp start_app() do
    Application.load(:livebook)
    Application.put_env(:livebook, :serverless, true)
    Application.ensure_all_started(:livebook)
  end
end
