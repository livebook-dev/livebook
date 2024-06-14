defmodule Livebook do
  @moduledoc """
  This module provides a public Elixir API for integrating with Livebook.

  ## Configuration

  See `config_runtime/0` for bootstrapping the default runtime
  configuration. There are several public configuration entries that
  you can customize.

  ### Custom plugs

  You can list a number of plugs to call directly before the Livebook
  router

      config :livebook, :plugs, [{CustomPlug, []}]

  ### Embedded runtime dependencies

  In case you use the Embedded runtime and support installing
  dependencies with `Mix.install/2`, you can make those discoverable
  in the package search, by configuring a loader function:

      config :livebook, Livebook.Runtime.Embedded,
        load_packages: {Loader, :packages, []}

  The function should return a list of entries like this:

      [
        %{
          dependency: %{dep: {:kino, "~> 0.6.1"}, config: []},
          description: "Interactive widgets for Livebook",
          name: "kino",
          url: "https://hex.pm/packages/kino",
          version: "0.6.1"
        }
      ]

  ### Custom learn notebooks

  **Note that this is compile time configuration.**

  A list of additional notebooks to include in the Learn section.

  Note that the notebooks are loaded and embedded in a compiled module,
  so the paths are accessed at compile time only.

      config :livebook, :learn_notebooks, [
        %{
          # Required notebook path
          path: "/path/to/notebook.livemd",
          # Optional notebook identifier for URLs, as in /learn/notebooks/{slug}
          # By default the slug is inferred from file name, so there is no need to set it
          slug: "my-notebook"
          # Optional list of images
          image_paths: [
            # This image can be sourced as images/myimage.jpg in the notebook
            "/path/to/myimage.jpg"
          ],
          # Optional details for the notebook card. If omitted, the notebook
          # is hidden in the UI, but still accessible under /learn/notebooks/{slug}
          details: %{
            cover_path: "/path/to/logo.png",
            description: "My custom notebook that showcases some amazing stuff."
          }
        },
        %{
          path: "/path/to/other_notebook.livemd"
        }
      ]

  """

  @doc """
  Executes Livebook's `config/runtime.exs`.

  If you use Livebook as a dependency, you can add the following
  to your `config/runtime.exs` to trigger Livebook's `config/runtime.exs`
  configuration:

      Livebook.config_runtime()

  """
  def config_runtime do
    if root = System.get_env("RELEASE_ROOT") do
      for file <- Path.wildcard(Path.join(root, "user/extensions/*.exs")) do
        Code.require_file(file)
      end
    end

    import Config

    config :livebook, :random_boot_id, :crypto.strong_rand_bytes(3)

    config :livebook, LivebookWeb.Endpoint,
      secret_key_base:
        Livebook.Config.secret!("LIVEBOOK_SECRET_KEY_BASE") ||
          Livebook.Utils.random_secret_key_base()

    if Livebook.Config.debug!("LIVEBOOK_DEBUG") do
      config :logger, level: :debug
    end

    if port = Livebook.Config.port!("LIVEBOOK_PORT") do
      config :livebook, LivebookWeb.Endpoint, http: [port: port]
    end

    if ip = Livebook.Config.ip!("LIVEBOOK_IP") do
      host = Livebook.Utils.ip_to_host(ip)
      config :livebook, LivebookWeb.Endpoint, http: [ip: ip], url: [host: host]
    end

    if base_url_path = Livebook.Config.base_url_path!("LIVEBOOK_BASE_URL_PATH") do
      config :livebook, LivebookWeb.Endpoint, url: [path: base_url_path]
    end

    if password = Livebook.Config.password!("LIVEBOOK_PASSWORD") do
      config :livebook, :authentication, {:password, password}
    else
      case Livebook.Config.boolean!("LIVEBOOK_TOKEN_ENABLED", nil) do
        true -> config :livebook, :authentication, :token
        false -> config :livebook, :authentication, :disabled
        # Keep the environment-specific default
        nil -> :ok
      end
    end

    if port = Livebook.Config.port!("LIVEBOOK_IFRAME_PORT") do
      config :livebook, :iframe_port, port
    end

    if url = Livebook.Config.iframe_url!("LIVEBOOK_IFRAME_URL") do
      config :livebook, :iframe_url, url
    end

    if url = Livebook.Config.teams_url!("LIVEBOOK_TEAMS_URL") do
      config :livebook, teams_url: url, warn_on_live_teams_server: false
    end

    if Livebook.Config.boolean!("LIVEBOOK_SHUTDOWN_ENABLED", false) do
      config :livebook, :shutdown_callback, {System, :stop, []}
    end

    if Livebook.Config.boolean!("LIVEBOOK_WITHIN_IFRAME", false) do
      config :livebook, :within_iframe, true
    end

    if Livebook.Config.boolean!("LIVEBOOK_AWS_CREDENTIALS", false) do
      config :livebook, :aws_credentials, true
    end

    if Livebook.Config.boolean!("LIVEBOOK_EPMDLESS", false) do
      config :livebook, :epmdless, true
    end

    config :livebook,
           :default_runtime,
           Livebook.Config.default_runtime!("LIVEBOOK_DEFAULT_RUNTIME") ||
             Livebook.Runtime.ElixirStandalone.new()

    config :livebook, :default_app_runtime, Livebook.Runtime.ElixirStandalone.new()

    config :livebook,
           :runtime_modules,
           [
             Livebook.Runtime.ElixirStandalone,
             Livebook.Runtime.Attached
           ]

    if home = Livebook.Config.writable_dir!("LIVEBOOK_HOME") do
      config :livebook, :home, home
    end

    if data_path = Livebook.Config.writable_dir!("LIVEBOOK_DATA_PATH") do
      config :livebook, :data_path, data_path
    end

    if apps_path = System.get_env("LIVEBOOK_APPS_PATH") do
      config :livebook, :apps_path, apps_path
    end

    if apps_path_hub_id = System.get_env("LIVEBOOK_APPS_PATH_HUB_ID") do
      config :livebook, :apps_path_hub_id, apps_path_hub_id
    end

    if apps_path_password = Livebook.Config.password!("LIVEBOOK_APPS_PATH_PASSWORD") do
      config :livebook, :apps_path_password, apps_path_password
    end

    if apps_path_warmup = Livebook.Config.apps_path_warmup!("LIVEBOOK_APPS_PATH_WARMUP") do
      config :livebook, :apps_path_warmup, apps_path_warmup
    end

    if force_ssl_host = Livebook.Config.force_ssl_host!("LIVEBOOK_FORCE_SSL_HOST") do
      config :livebook, :force_ssl_host, force_ssl_host
    end

    if cacertfile = Livebook.Config.cacertfile!("LIVEBOOK_CACERTFILE") do
      config :livebook, :cacertfile, cacertfile
    end

    config :livebook, :rewrite_on, Livebook.Config.rewrite_on!("LIVEBOOK_PROXY_HEADERS")

    config :livebook,
           :cookie,
           Livebook.Config.cookie!("LIVEBOOK_COOKIE") || Livebook.Utils.random_cookie()

    # TODO: remove in v1.0
    if System.get_env("LIVEBOOK_DISTRIBUTION") == "sname" do
      IO.warn(
        ~s/Ignoring LIVEBOOK_DISTRIBUTION=sname, because short names are no longer supported./,
        []
      )
    end

    if node = Livebook.Config.node!("LIVEBOOK_NODE") do
      config :livebook, :node, node
    end

    if app_service_name = Livebook.Config.app_service_name!("LIVEBOOK_APP_SERVICE_NAME") do
      config :livebook, :app_service_name, app_service_name

      config :livebook,
             :app_service_url,
             Livebook.Config.app_service_url!("LIVEBOOK_APP_SERVICE_URL")
    end

    if update_instructions_url =
         Livebook.Config.update_instructions_url!("LIVEBOOK_UPDATE_INSTRUCTIONS_URL") do
      config :livebook, :update_instructions_url, update_instructions_url
    end

    if allowed_uri_schemes = Livebook.Config.allowed_uri_schemes!("LIVEBOOK_ALLOW_URI_SCHEMES") do
      config :livebook, :allowed_uri_schemes, allowed_uri_schemes
    end

    config :livebook,
           :identity_provider,
           Livebook.Config.identity_provider!("LIVEBOOK_IDENTITY_PROVIDER")

    if dns_cluster_query = Livebook.Config.dns_cluster_query!("LIVEBOOK_CLUSTER") do
      config :livebook, :dns_cluster_query, dns_cluster_query
    end

    if agent_name = Livebook.Config.agent_name!("LIVEBOOK_AGENT_NAME") do
      config :livebook, :agent_name, agent_name
    end

    if Livebook.Config.boolean!("LIVEBOOK_FIPS", false) do
      if :crypto.enable_fips_mode(true) do
        IO.puts("[Livebook] FIPS mode enabled")
      else
        Livebook.Config.abort!(
          "Requested FIPS mode via LIVEBOOK_FIPS, but this Erlang installation was compiled without FIPS support"
        )
      end
    end
  end

  @doc """
  Parses the given Live Markdown document and converts it to Elixir
  source code.

  ## Limitations

  Note that the resulting script may not compile in some cases, for
  example if you define a macro in one cell and import it in another
  cell, it works fine in Livebook, because each cell is compiled
  separately. However, when running the script it gets compiled as a
  whole and consequently doing so doesn't work.

  Additionally, branching sections are commented out.
  """
  @spec live_markdown_to_elixir(String.t()) :: String.t()
  def live_markdown_to_elixir(markdown) do
    {notebook, _info} = Livebook.LiveMarkdown.notebook_from_livemd(markdown)
    Livebook.Notebook.Export.Elixir.notebook_to_elixir(notebook)
  end
end
