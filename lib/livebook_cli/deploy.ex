defmodule LivebookCLI.Deploy do
  @moduledoc """
  Usage: livebook deploy [options] filename|directory

  ## Available options

    --deploy-key   Sets the deploy key to authenticate with Livebook Teams
    --teams-key    Sets the Teams key to encrypt the Livebook app
    --dg           The deployment group name which you want to deploy

  The --help option can be given to print this notice.

  ## Examples

  Deploys a single notebook:

      livebook deploy --deploy-key="lb_dk_..." --teams-key="lb_tk_..." -dg "online" path/to/file.livemd

  Deploys a folder:

      livebook deploy --deploy-key="lb_dk_..." --teams-key="lb_tk_..." -dg "online" path/to\
  """
  use LivebookCLI.Command

  @impl true
  def usage() do
    @moduledoc
  end

  @switches [
    deploy_key: :string,
    teams_key: :string,
    dg: :string
  ]

  @impl true
  def call(args) do
    Application.put_env(:livebook, :mode, :cli)

    {:ok, _} = Application.ensure_all_started(:livebook)
    config = config_from_args(args)

    with :ok <- validate_config(config),
         {:ok, attrs} <- authenticate_cli(config) do
      team = fetch_or_create_hub(attrs, config)
      deploy_to_teams(team, config, attrs)
    end
  end

  defp config_from_args(args) do
    {opts, filename_or_directory} = OptionParser.parse!(args, strict: @switches)
    filename_or_directory = Path.expand(filename_or_directory)

    %{
      teams_key: opts[:teams_key],
      deploy_key: opts[:deploy_key],
      deployment_group_name: opts[:dg],
      path: filename_or_directory
    }
  end

  defp validate_config(config) do
    debug("Validating config from options...")

    cond do
      config.deploy_key == nil or not String.starts_with?(config.deploy_key, "lb_dk_") ->
        {:error, "must be a Livebook Teams Deploy Key"}

      config.teams_key == nil or not String.starts_with?(config.teams_key, "lb_tk_") ->
        {:error, "must be a Livebook Teams Key"}

      config.deployment_group_name in ["", nil] ->
        {:error, "must be a deployment group name"}

      not File.exists?(config.path) ->
        {:error, "must be a valid path"}

      :else ->
        :ok
    end
  end

  defp authenticate_cli(config) do
    info("Authenticating CLI...")

    case Req.post(client(config), url: "/api/v1/auth-cli") do
      {:ok, %{status: 200, body: body}} -> {:ok, body}
      {:ok, %{status: 401}} -> {:error, "Invalid API credentials"}
      {:ok, %{status: 403}} -> {:error, "You don't have access to this action"}
      {:ok, %{status: 500}} -> {:error, "Something went wrong"}
      {:ok, %{status: _, body: body}} -> {:error, error_from_body(body)}
      {:error, exception} -> raise exception
    end
  end

  defp client(config) do
    debug("Building Req client for: #{inspect(config, pretty: true)}")

    key_hash = Livebook.Teams.Org.key_hash(config.teams_key)
    token = "#{config.deploy_key}:#{key_hash}:#{config.deployment_group_name}"

    Req.new(base_url: Livebook.Config.teams_url(), auth: {:bearer, token})
    |> Req.Request.put_new_header("x-lb-version", Livebook.Config.app_version())
    |> Livebook.Utils.req_attach_defaults()
  end

  defp error_from_body(%{"errors" => %{"detail" => error}}) do
    "Livebook Teams API returned error: #{error}"
  end

  defp error_from_body(error) when is_binary(error) do
    "Livebook Teams API returned error: #{error}"
  end

  defp fetch_or_create_hub(%{"name" => name} = attrs, config) do
    id = "team-#{name}"

    if not Livebook.Hubs.hub_exists?(id) do
      Livebook.Hubs.save_hub(%Livebook.Hubs.Team{
        id: id,
        hub_name: name,
        hub_emoji: "🚀",
        user_id: nil,
        org_id: attrs["org_id"],
        org_key_id: attrs["org_key_id"],
        session_token: "#{config.deploy_key}:#{config.deployment_group_name}",
        teams_key: config.teams_key,
        org_public_key: attrs["public_key"]
      })

      Application.put_env(:livebook, :teams_auth, :online)
      Application.put_env(:livebook, :apps_path_hub_id, id)
    end

    Livebook.Hubs.fetch_hub!(id)
  end

  defp deploy_to_teams(team, config, %{"url" => url}) do
    for path <- list_notebooks(config.path),
        String.ends_with?(path, ".livemd") do
      deploy_notebook(team, path, url)
    end

    :ok
  end

  defp list_notebooks(path) do
    debug("Listing notebooks from: #{path}")

    files =
      if File.dir?(path) do
        path
        |> File.ls!()
        |> Enum.map(&Path.join(path, &1))
        |> Enum.reject(&File.dir?/1)
      else
        [path]
      end

    if files == [] do
      raise "There's no notebook available to deploy"
    else
      if length(files) == 1 do
        debug("Found 1 notebook")
      else
        debug("Found #{length(files)} notebooks")
      end

      files
    end
  end

  defp deploy_notebook(team, path, url) do
    info("Deploying notebook: #{path}")
    files_dir = Livebook.FileSystem.File.local(path)

    with {:ok, content} <- File.read(path),
         {:ok, notebook} = notebook_from_livemd(content),
         {:ok, app_deployment} <- prepare_app_deployment(notebook, files_dir) do
      case Livebook.Teams.deploy_app(team, app_deployment) do
        :ok -> print_deployment(app_deployment, url)
        {:error, _changeset} -> raise "Invalid data"
        {:transport_error, reason} -> raise reason
      end
    end
  end

  defp notebook_from_livemd(content) do
    debug("Loading notebook from content")

    case Livebook.LiveMarkdown.notebook_from_livemd(content) do
      {notebook, %{warnings: [], stamp_verified?: true}} ->
        debug("Notebook loaded for app: #{notebook.app_settings.slug}")
        {:ok, notebook}

      {_notebook, %{warnings: [], stamp_verified?: false}} ->
        {:error, "Failed to validate the notebook stamp"}

      {_notebook, %{warnings: warnings}} ->
        {:error, Enum.join(warnings, "\n")}
    end
  end

  defp prepare_app_deployment(notebook, files_dir) do
    debug("Preparing app deployment for #{notebook.app_settings.slug}")

    case Livebook.Teams.AppDeployment.new(notebook, files_dir) do
      {:ok, app_deployment} -> {:ok, app_deployment}
      {:warning, warnings} -> {:error, Enum.join(warnings, "\n")}
      other -> other
    end
  end

  defp print_deployment(app_deployment, url) do
    slug =
      if url do
        "#{app_deployment.slug} (#{url}/apps/#{app_deployment.slug})"
      else
        app_deployment.slug
      end

    info("""
    App deployment created successfully.

    Slug: #{slug}
    Title: #{app_deployment.title}
    """)
  end
end
