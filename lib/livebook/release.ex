defmodule Livebook.Release do
  @moduledoc false

  @doc """
  Runs the setup for all apps deployed from directory on startup.
  """
  def warmup_apps() do
    start_app()

    if apps_path = Livebook.Config.apps_path() do
      case Livebook.Config.apps_path_warmup() do
        :manual ->
          :ok

        other ->
          Livebook.Config.abort!(
            "expected apps warmup mode to be :manual, got: #{inspect(other)}." <>
              " Make sure to set LIVEBOOK_APPS_PATH_WARMUP=manual"
          )
      end

      Livebook.Apps.deploy_apps_in_dir(apps_path, warmup: true, skip_deploy: true)
    end
  end

  defp start_app() do
    Application.load(:livebook)
    Application.put_env(:livebook, :serverless, true)
    {:ok, _} = Application.ensure_all_started(:livebook)
  end
end
