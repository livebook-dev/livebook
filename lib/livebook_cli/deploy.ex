defmodule LivebookCLI.Deploy do
  import LivebookCLI.Utils
  alias Livebook.Teams

  @behaviour LivebookCLI.Task

  @deploy_key_prefix Teams.Requests.deploy_key_prefix()
  @teams_key_prefix Teams.Org.teams_key_prefix()

  @impl true
  def usage() do
    """
    Usage: livebook deploy [options] filename|directory

    ## Available options

      --deploy-key        Sets the deploy key to authenticate with Livebook Teams
      --teams-key         Sets the Teams key to authenticate with Livebook Teams and encrypt the Livebook app
      --deployment-group  The deployment group name which you want to deploy

    The --help option can be given to print this notice.

    ## Examples

    Deploys a single notebook:

        livebook deploy --deploy-key="lb_dk_..." --teams-key="lb_tk_..." -deployment-group "online" path/to/file.livemd

    Deploys a folder:

        livebook deploy --deploy-key="lb_dk_..." --teams-key="lb_tk_..." -deployment-group "online" path/to\
    """
  end

  @switches [
    deploy_key: :string,
    teams_key: :string,
    deployment_group: :string
  ]

  @impl true
  def call(args) do
    Application.put_env(:livebook, :mode, :cli)

    {:ok, _} = Application.ensure_all_started(:livebook)
    config = config_from_args(args)
    ensure_config!(config)

    team = authenticate_cli!(config)
    deploy_to_teams(team, config)
  end

  defp config_from_args(args) do
    {opts, filename_or_directory} = OptionParser.parse!(args, strict: @switches)
    filename_or_directory = Path.expand(filename_or_directory)

    %{
      path: filename_or_directory,
      session_token: opts[:deploy_key],
      teams_key: opts[:teams_key],
      deployment_group: opts[:deployment_group]
    }
  end

  defp ensure_config!(config) do
    log_debug("Validating config from options...")

    errors =
      Enum.reduce(config, %{}, fn
        {:session_token, value}, acc when value in ["", nil] ->
          add_error(acc, "Deploy Key", "can't be blank")

        {:session_token, value}, acc ->
          if not String.starts_with?(value, @deploy_key_prefix) do
            add_error(acc, "Deploy Key", "must be a Livebook Teams Deploy Key")
          else
            acc
          end

        {:teams_key, value}, acc when value in ["", nil] ->
          add_error(acc, "Teams Key", "can't be blank")

        {:teams_key, value}, acc ->
          if not String.starts_with?(value, @teams_key_prefix) do
            add_error(acc, "Teams Key", "must be a Livebook Teams Key")
          else
            acc
          end

        {:deployment_group, value}, acc when value in ["", nil] ->
          add_error(acc, "Deployment Group", "can't be blank")

        {:path, value}, acc when value in ["", nil] ->
          add_error(acc, "Path", "can't be blank")

        {:path, value}, acc ->
          if not File.exists?(value) do
            add_error(acc, "Path", "must be a valid path")
          else
            acc
          end

        _otherwise, acc ->
          acc
      end)

    if Map.keys(errors) == [] do
      :ok
    else
      raise """
      You configuration is invalid, make sure you are using the correct options for this task.

      #{format_errors(errors)}\
      """
    end
  end

  defp authenticate_cli!(config) do
    log_debug("Authenticating CLI...")

    case Teams.fetch_cli_session(config) do
      {:ok, team} -> team
      {:error, error} -> raise error
      {:transport_error, error} -> raise error
    end
  end

  defp deploy_to_teams(team, config) do
    for path <- list_notebooks!(config.path) do
      log_debug("Deploying notebook: #{path}")
      files_dir = Livebook.FileSystem.File.local(path)

      with {:ok, content} <- File.read(path),
           {:ok, app_deployment} <- prepare_app_deployment(path, content, files_dir) do
        case Livebook.Teams.deploy_app_from_cli(team, app_deployment, config.deployment_group) do
          {:ok, url} -> print_deployment(app_deployment, url)
          {:error, errors} -> raise format_errors(errors)
          {:transport_error, reason} -> raise reason
        end
      end
    end

    :ok
  end

  defp list_notebooks!(path) do
    log_debug("Listing notebooks from: #{path}")

    files =
      if File.dir?(path) do
        path
        |> File.ls!()
        |> Enum.map(&Path.join(path, &1))
        |> Enum.reject(&File.dir?/1)
        |> Enum.filter(&String.ends_with?(&1, ".livemd"))
      else
        [path]
      end

    if files == [] do
      raise "There's no notebook available to deploy"
    else
      if length(files) == 1 do
        log_debug("Found 1 notebook")
      else
        log_debug("Found #{length(files)} notebooks")
      end

      files
    end
  end

  defp add_error(errors, key, message) do
    Map.update(errors, key, [message], &[message | &1])
  end

  defp format_errors(%{} = errors_map) do
    errors_map
    |> Enum.map(fn {key, errors} ->
      """
      * #{key}
      #{format_list(errors)}\
      """
    end)
    |> Enum.join("\n")
  end

  defp format_list(errors) when is_list(errors) do
    errors |> Enum.map(&"  * #{&1}") |> Enum.join("\n")
  end

  defp prepare_app_deployment(path, content, files_dir) do
    case Livebook.Teams.AppDeployment.new(content, files_dir) do
      {:ok, app_deployment} ->
        {:ok, app_deployment}

      {:warning, warnings} ->
        raise """
        Deployment for notebook #{Path.basename(path)} failed because the notebook has some warnings:
        #{format_list(warnings)}
        """

      {:error, reason} ->
        raise "Failed to handle I/O operations: #{reason}"
    end
  end

  defp print_deployment(app_deployment, url) do
    print_text([
      :green,
      "App deployment created successfully.\n\n",
      :magenta,
      :bright,
      "Slug: ",
      :reset,
      :white,
      "#{app_deployment.slug} (#{url})\n",
      :magenta,
      :bright,
      "Title: ",
      :reset,
      :white,
      app_deployment.title
    ])
  end
end
