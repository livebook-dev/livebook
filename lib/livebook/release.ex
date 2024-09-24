defmodule Livebook.Release do
  require Logger

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

      app_specs =
        Livebook.Apps.build_app_specs_in_dir(apps_path,
          hub_id: Livebook.Config.apps_path_hub_id()
        )

      if app_specs != [] do
        Logger.info("Running app warmups")

        for app_spec <- app_specs do
          with {:error, message} <- Livebook.Apps.warmup_app(app_spec) do
            Logger.info("Warmup failed for app #{app_spec.slug}, #{message}")
          end
        end
      end
    end
  end

  defp start_app() do
    Application.load(:livebook)
    Application.put_env(:livebook, :serverless, true)
    {:ok, _} = Application.ensure_all_started(:livebook)
  end
end
