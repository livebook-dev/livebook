defmodule LivebookCLI.Deploy do
  import LivebookCLI.Utils
  alias Livebook.Teams

  @behaviour LivebookCLI.Task

  @deploy_key_prefix Teams.Constants.deploy_key_prefix()
  @teams_key_prefix Teams.Constants.teams_key_prefix()

  @impl true
  def usage() do
    """
    Usage: livebook deploy [options] filename [filename...]

    ## Available options

      --deploy-key        Deploy key from your Livebook Teams organization
      --teams-key         Teams key from your Teams workspace
      --deployment-group  The deployment group name which you want to deploy to

    The --help option can be given to print this notice.

    ## Examples

    Deploys a single notebook:

        livebook deploy --deploy-key="lb_dk_..." --teams-key="lb_tk_..." --deployment-group="production" path/to/app1.livemd

    Deploys multiple notebooks:

        livebook deploy --deploy-key="lb_dk_..." --teams-key="lb_tk_..." --deployment-group="production" path/to/*.livemd\
    """
  end

  @switches [
    deploy_key: :string,
    teams_key: :string,
    deployment_group: :string
  ]

  @impl true
  def call(args) do
    Application.put_env(:livebook, :persist_storage, false)
    {:ok, _} = Application.ensure_all_started(:livebook)
    config = config_from_args(args)
    ensure_config!(config)

    team = authenticate_cli!(config)
    deploy_to_teams(team, config)
  end

  defp config_from_args(args) do
    {opts, paths} = OptionParser.parse!(args, strict: @switches)

    %{
      paths: paths,
      session_token: opts[:deploy_key],
      teams_key: opts[:teams_key],
      deployment_group: opts[:deployment_group]
    }
  end

  defp ensure_config!(config) do
    log_debug("Validating config from options...")

    errors =
      Enum.reduce(config, %{}, fn
        {key, value}, acc when value in ["", nil] ->
          add_error(acc, normalize_key(key), "can't be blank")

        {:session_token, value}, acc ->
          if not String.starts_with?(value, @deploy_key_prefix) do
            add_error(acc, normalize_key(:session_token), "must be a Livebook Teams Deploy Key")
          else
            acc
          end

        {:teams_key, value}, acc ->
          if not String.starts_with?(value, @teams_key_prefix) do
            add_error(acc, normalize_key(:teams_key), "must be a Livebook Teams Key")
          else
            acc
          end

        {:paths, values}, acc ->
          Enum.reduce_while(values, acc, &validate_path/2)

        _otherwise, acc ->
          acc
      end)

    if Map.keys(errors) == [] do
      :ok
    else
      raise LivebookCLI.Error, """
      You configuration is invalid, make sure you are using the correct options for this task.

      #{format_errors(errors, " * ")}\
      """
    end
  end

  defp validate_path(value, acc) do
    cond do
      not File.exists?(value) ->
        {:halt, add_error(acc, normalize_key(:paths), "must be a valid path")}

      File.dir?(value) ->
        {:halt, add_error(acc, normalize_key(:paths), "must be a file path")}

      true ->
        {:cont, acc}
    end
  end

  defp authenticate_cli!(config) do
    log_debug("Authenticating CLI...")

    case Teams.fetch_cli_session(config) do
      {:ok, team} -> team
      {:error, error} -> raise LivebookCLI.Error, error
      {:transport_error, error} -> raise LivebookCLI.Error, error
    end
  end

  defp deploy_to_teams(team, config) do
    if length(config.paths) == 1 do
      log_debug("Found 1 notebook")
    else
      log_debug("Found #{length(config.paths)} notebooks")
    end

    log_info("Deploying notebooks:")

    deploy_results =
      for path <- config.paths do
        log_info(" * Preparing to deploy notebook #{Path.basename(path)}")
        files_dir = Livebook.FileSystem.File.local(path)

        with {:ok, content} <- File.read(path),
             {:ok, app_deployment} <- prepare_app_deployment(path, content, files_dir) do
          case Livebook.Teams.deploy_app_from_cli(team, app_deployment, config.deployment_group) do
            {:ok, url} ->
              log_info([:green, "  * #{app_deployment.title} deployed successfully. (#{url})"])
              :ok

            {:error, errors} ->
              log_error("  * #{app_deployment.title} failed to deploy.")

              error_message =
                errors
                |> normalize_errors
                |> format_errors("    * ")

              log_error(error_message)

              :error

            {:transport_error, reason} ->
              log_error(
                "  * #{app_deployment.title} failed to deploy. Transport error: #{reason}"
              )

              :error
          end
        end
      end

    if Enum.any?(deploy_results, fn result -> result != :ok end) do
      raise LivebookCLI.Error, "Some app deployments failed."
    else
      :ok
    end
  end

  defp prepare_app_deployment(path, content, files_dir) do
    case Livebook.Teams.AppDeployment.new(content, files_dir) do
      {:ok, app_deployment} ->
        {:ok, app_deployment}

      {:warning, warnings} ->
        error_message = """
          * Deployment for notebook #{Path.basename(path)} failed because the notebook has some warnings:
          #{format_list(warnings, " * ")}
        """

        log_error(error_message)

        :error

      {:error, reason} ->
        log_error("  * Failed to handle I/O operations: #{reason}")

        :error
    end
  end

  defp add_error(errors, key, message) do
    Map.update(errors, key, [message], &[message | &1])
  end

  def normalize_errors(%{} = errors) do
    for {key, values} <- errors, into: %{} do
      {normalize_key(key), values}
    end
  end

  defp normalize_key(key) when is_atom(key), do: to_string(key) |> normalize_key()
  defp normalize_key("session_token"), do: "Deploy Key"
  defp normalize_key("teams_key"), do: "Teams Key"
  defp normalize_key("deployment_group"), do: "Deployment Group"
  defp normalize_key("paths"), do: "File Paths"

  defp format_errors(errors, prefix) do
    errors
    |> Enum.map(fn {key, values} ->
      values |> Enum.map(&"#{prefix}#{key} #{&1}") |> Enum.join("\n")
    end)
    |> Enum.join("\n")
  end

  defp format_list(errors, prefix) do
    errors |> Enum.map(&"#{prefix}#{&1}") |> Enum.join("\n")
  end
end
