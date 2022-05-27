defmodule Livebook do
  @moduledoc """
  Livebook is an interactive notebook system for Elixir.

  This module includes the public API.

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
          dependency: {:kino, "~> 0.6.1"},
          description: "Interactive widgets for Livebook",
          name: "kino",
          url: "https://hex.pm/packages/kino",
          version: "0.6.1"
        }
      ]

  ### Custom explore notebooks

  **Note that this is compile time configuration.**

  A list of additional notebooks to include in the Explore section.

  Note that the notebooks are loaded and embedded in a compiled module,
  so the paths are accessed at compile time only.

      config :livebook, :explore_notebooks, [
        %{
          # Required notebook path
          path: "/path/to/notebook.livemd",
          # Optional notebook identifier for URLs, as in /explore/notebooks/{slug}
          # By default the slug is inferred from file name, so there is no need to set it
          slug: "my-notebook"
          # Optional list of images
          image_paths: [
            # This image can be sourced as images/myimage.jpg in the notebook
            "/path/to/myimage.jpg"
          ],
          # Optional details for the notebook card. If omitted, the notebook
          # is hidden in the UI, but still accessible under /explore/notebooks/{slug}
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
    import Config

    config :livebook, LivebookWeb.Endpoint,
      secret_key_base:
        Livebook.Config.secret!("LIVEBOOK_SECRET_KEY_BASE") ||
          Base.encode64(:crypto.strong_rand_bytes(48))

    if port = Livebook.Config.port!("LIVEBOOK_PORT") do
      config :livebook, LivebookWeb.Endpoint, http: [port: port]
    end

    if ip = Livebook.Config.ip!("LIVEBOOK_IP") do
      host = Livebook.Utils.ip_to_host(ip)
      config :livebook, LivebookWeb.Endpoint, http: [ip: ip], url: [host: host]
    end

    cond do
      password = Livebook.Config.password!("LIVEBOOK_PASSWORD") ->
        config :livebook, authentication_mode: :password, password: password

      Livebook.Config.boolean!("LIVEBOOK_TOKEN_ENABLED", true) ->
        config :livebook, token: Livebook.Utils.random_id()

      true ->
        config :livebook, authentication_mode: :disabled
    end

    if port = Livebook.Config.port!("LIVEBOOK_IFRAME_PORT") do
      config :livebook, :iframe_port, port
    end

    if url = Livebook.Config.iframe_url!("LIVEBOOK_IFRAME_URL") do
      config :livebook, :iframe_url, url
    end

    if Livebook.Config.boolean!("LIVEBOOK_SHUTDOWN_ENABLED", false) do
      config :livebook, :shutdown_enabled, true
    end

    config :livebook,
           :default_runtime,
           Livebook.Config.default_runtime!("LIVEBOOK_DEFAULT_RUNTIME") ||
             Livebook.Runtime.ElixirStandalone.new()

    config :livebook,
           :runtime_modules,
           [
             Livebook.Runtime.ElixirStandalone,
             Livebook.Runtime.MixStandalone,
             Livebook.Runtime.Attached
           ]

    if home = Livebook.Config.writable_dir!("LIVEBOOK_HOME") do
      config :livebook, :home, home
    end

    if data_path = Livebook.Config.writable_dir!("LIVEBOOK_DATA_PATH") do
      config :livebook, :data_path, data_path
    end

    if force_ssl_host = Livebook.Config.force_ssl_host!("LIVEBOOK_FORCE_SSL_HOST") do
      config :livebook, :force_ssl_host, force_ssl_host
    end

    config :livebook,
           :cookie,
           Livebook.Config.cookie!("LIVEBOOK_COOKIE") ||
             Livebook.Config.cookie!("RELEASE_COOKIE") ||
             Livebook.Utils.random_cookie()

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
    {notebook, _messages} = Livebook.LiveMarkdown.notebook_from_livemd(markdown)
    Livebook.Notebook.Export.Elixir.notebook_to_elixir(notebook)
  end
end
