defmodule LivebookCLI.Deploy do
  use LivebookCLI.Command

  @impl true
  def usage() do
    """
    Usage: livebook deploy [options] filename|directory

    ## Available options

      --api-key      Sets the API key to authenticate with Livebook Teams
      --teams-key    Sets the Teams key to encrypt the Livebook app
      --dg           The deployment group name which you want to deploy

    The --help option can be given to print this notice.

    ## Environment variables

    The following environment variables can be used to configure Livebook on boot:

      * `LIVEBOOK_TEAMS_API_KEY` - sets the API key to authenticate with Livebook Teams

      * `LIVEBOOK_TEAMS_KEY` - sets the Teams key to encrypt the Livebook app

    ## Examples

    Deploys a single notebook:

        livebook deploy --api-key="lb_ak_..." --teams-key="lb_tk_..." -dg "online" path/to/file.livemd

    Deploys a folder:

        livebook deploy --api-key="lb_ak_..." --teams-key="lb_tk_..." -dg "online" path/to

    Deploys from environment variable:

        export LIVEBOOK_TEAMS_KEY=lb_tk...
        export LIVEBOOK_TEAMS_API_KEY=lb_ak_...
        livebook deploy -dg "online" path/to/file.livemd

    Deploys an imported notebook at the given URL:

        livebook deploy --api-key="lb_ak_..." --teams-key="lb_tk_..." -dg "online" https://example.com/my-notebook.livemd\
    """
  end

  @switches [
    api_key: :string,
    teams_key: :string,
    dg: :string
  ]

  @impl true
  def call(args) do
    config = config_from_args(args)

    with :ok <- validate_config(config),
         {:ok, session_auth} <- authenticate_cli(config) do
      deploy(session_auth, config.path)
    end
  end

  defp config_from_args(args) do
    {opts, filename_or_directory} = OptionParser.parse!(args, strict: @switches)
    Enum.into(opts, %{path: filename_or_directory})
  end

  defp validate_config(config) do
    cond do
      config[:api_key] == nil or not String.starts_with?(config.api_key, "lb_ak_") ->
        {:error, "must be a Livebook Teams API Key"}

      config[:teams_key] == nil or not String.starts_with?(config.teams_key, "lb_tk_") ->
        {:error, "must be a Livebook Teams Key"}

      config[:dg] in ["", nil] ->
        {:error, "must be a deployment group name"}

      not File.exists?(config.path) ->
        {:error, "must be a valid path"}

      :else ->
        :ok
    end
  end

  defp authenticate_cli(config) do
    req = Req.new(base_url: Livebook.Config.teams_url(), auth: {:bearer, config.api_key})
    json = %{key_hash: Livebook.Teams.Org.key_hash(config), deployment_group_name: config.dg}

    case Req.post(req, path: "/api/v1/auth-cli", json: json) do
      {:ok, %{status: 200, body: session_auth}} -> {:ok, session_auth}
      {:ok, %{status: _, body: body}} -> {:error, error_from_body(body)}
      {:error, exception} -> Kernel.raise(exception)
    end
  end

  defp error_from_body(%{"error" => %{"details" => error}}) do
    "Livebook Teams API returned error: #{error}"
  end

  defp deploy(session_auth, path) do
    for filename <- File.ls(path),
        path = Path.join(path, filename),
        not File.dir?(path),
        String.ends_with?(path, ".livemd") do
      {notebook, %{warnings: []}} = Livebook.LiveMarkdown.notebook_from_livemd(source)
    end

    :ok
  end
end
