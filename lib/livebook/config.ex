defmodule Livebook.Config do
  alias Livebook.FileSystem

  @type authentication ::
          %{mode: :password, secret: String.t()}
          | %{mode: :token, secret: String.t()}
          | %{mode: :disabled}

  @doc """
  Returns path to Livebook priv directory.

  This returns the usual priv directory, however in case of Escript,
  the priv files are extracted into a temporary directory and that is
  the path returned.
  """
  @spec priv_path() :: String.t()
  def priv_path() do
    Application.get_env(:livebook, :priv_dir) || Application.app_dir(:livebook, "priv")
  end

  @doc """
  Returns docker images to be used when generating sample Dockerfiles.
  """
  @spec docker_images() ::
          list(%{
            tag: String.t(),
            name: String.t(),
            env: list({String.t(), String.t()})
          })
  def docker_images() do
    version = app_version()

    version = if version =~ "dev", do: "nightly", else: version

    [
      %{tag: version, name: "Livebook", env: []},
      %{tag: "#{version}-cuda12", name: "Livebook + CUDA 12", env: []}
    ]
  end

  @doc """
  Returns the default runtime.
  """
  @spec default_runtime() :: Livebook.Runtime.t()
  def default_runtime() do
    Application.fetch_env!(:livebook, :default_runtime)
  end

  @doc """
  Returns the default runtime for app sessions.
  """
  @spec default_app_runtime() :: Livebook.Runtime.t()
  def default_app_runtime() do
    Application.fetch_env!(:livebook, :default_app_runtime)
  end

  @doc """
  Returns if the runtime module is enabled.
  """
  @spec runtime_enabled?(module()) :: boolean()
  def runtime_enabled?(runtime) do
    runtime in Application.fetch_env!(:livebook, :runtime_modules) or
      runtime == default_runtime().__struct__
  end

  @doc """
  Returns the authentication configuration.
  """
  @spec authentication() :: authentication()
  def authentication() do
    case Application.fetch_env!(:livebook, :authentication) do
      {:password, password} -> %{mode: :password, secret: password}
      :token -> %{mode: :token, secret: auth_token()}
      :disabled -> %{mode: :disabled}
    end
  end

  @auth_token_key {__MODULE__, :auth_token}

  defp auth_token() do
    if token = :persistent_term.get(@auth_token_key, nil) do
      token
    else
      token = Livebook.Utils.random_long_id()
      :persistent_term.put(@auth_token_key, token)
      token
    end
  end

  @doc """
  Returns the local file system.
  """
  @spec local_file_system() :: FileSystem.t()
  def local_file_system do
    :persistent_term.get(:livebook_local_file_system)
  end

  @doc """
  Returns the local file system home.
  """
  @spec local_file_system_home() :: FileSystem.File.t()
  def local_file_system_home do
    FileSystem.File.new(local_file_system())
  end

  @doc """
  Returns the home path.
  """
  @spec home() :: String.t()
  def home do
    Application.get_env(:livebook, :home) || user_home() || File.cwd!()
  end

  defp user_home(), do: System.user_home() |> Path.expand()

  @doc """
  Returns the configuration path.
  """
  @spec data_path() :: String.t()
  def data_path() do
    Application.get_env(:livebook, :data_path) || :filename.basedir(:user_data, "livebook")
  end

  @doc """
  Returns path to Livebook temporary dir.
  """
  @spec tmp_path() :: String.t()
  def tmp_path() do
    tmp_dir = System.tmp_dir!() |> Path.expand()
    Path.join([tmp_dir, "livebook", app_version()])
  end

  @doc """
  Returns the apps path.
  """
  @spec apps_path() :: String.t() | nil
  def apps_path() do
    Application.get_env(:livebook, :apps_path)
  end

  @doc """
  Returns the password configured for all apps deployed from `apps_path`.
  """
  @spec apps_path_password() :: String.t() | nil
  def apps_path_password() do
    Application.get_env(:livebook, :apps_path_password)
  end

  @doc """
  Returns the hub configured for all apps deployed from `apps_path`.
  """
  @spec apps_path_hub_id() :: String.t() | nil
  def apps_path_hub_id() do
    Application.get_env(:livebook, :apps_path_hub_id)
  end

  @doc """
  Returns warmup mode for apps deployed from dir.
  """
  @spec apps_path_warmup() :: :auto | :manual
  def apps_path_warmup() do
    Application.get_env(:livebook, :apps_path_warmup, :auto)
  end

  @doc """
  Returns the configured port for the Livebook endpoint.

  Note that the value may be `0`.
  """
  @spec port() :: pos_integer() | 0
  def port() do
    Application.get_env(:livebook, LivebookWeb.Endpoint)[:http][:port]
  end

  @doc """
  Returns the configured port for the iframe endpoint.
  """
  @spec iframe_port() :: pos_integer() | 0
  def iframe_port() do
    case port() do
      0 -> 0
      _ -> Application.fetch_env!(:livebook, :iframe_port)
    end
  end

  @doc """
  Returns the configured URL for the iframe endpoint.
  """
  @spec iframe_url() :: String.t() | nil
  def iframe_url() do
    Application.get_env(:livebook, :iframe_url)
  end

  @doc """
  Returns if this instance is running with teams auth,
  i.e. if there an online or offline hub created on boot.
  """
  @spec teams_auth?() :: boolean()
  def teams_auth?() do
    Application.fetch_env!(:livebook, :teams_auth?)
  end

  @doc """
  Returns the configured URL for the Livebook Teams endpoint.
  """
  @spec teams_url() :: String.t()
  def teams_url() do
    Application.fetch_env!(:livebook, :teams_url)
  end

  @doc """
  Returns the configured name for the Livebook Agent session.
  """
  @spec agent_name() :: String.t()
  def agent_name() do
    Application.fetch_env!(:livebook, :agent_name)
  end

  @doc """
  Returns if aws_credentials is enabled.
  """
  @spec aws_credentials?() :: boolean()
  def aws_credentials?() do
    Application.fetch_env!(:livebook, :aws_credentials)
  end

  @doc """
  Shuts down the system, if possible.
  """
  def shutdown do
    case Livebook.Config.shutdown_callback() do
      {m, f, a} ->
        Phoenix.PubSub.broadcast(Livebook.PubSub, "sidebar", :shutdown)
        apply(m, f, a)

      nil ->
        :ok
    end
  end

  @doc """
  Returns an mfa if there's a way to shut down the system.
  """
  @spec shutdown_callback() :: {module(), atom(), list()} | nil
  def shutdown_callback() do
    Application.fetch_env!(:livebook, :shutdown_callback)
  end

  @doc """
  Returns the identity provider.
  """
  @spec identity_provider() :: {atom(), module, binary}
  def identity_provider() do
    case Application.fetch_env(:livebook, :identity_provider) do
      {:ok, result} -> result
      :error -> {:session, Livebook.ZTA.PassThrough, :unused}
    end
  end

  @identity_provider_no_id [Livebook.ZTA.BasicAuth, Livebook.ZTA.PassThrough]

  @doc """
  Returns if the identity data is readonly.
  """
  @spec identity_provider_read_only?() :: boolean()
  def identity_provider_read_only?() do
    {_type, module, _key} = Livebook.Config.identity_provider()
    module not in @identity_provider_no_id
  end

  @doc """
  Returns if the identity provider supports logout.
  """
  @spec logout_enabled?() :: boolean()
  def logout_enabled?() do
    {_type, module, _key} = Livebook.Config.identity_provider()

    Code.ensure_loaded?(module) and function_exported?(module, :logout, 2)
  end

  @doc """
  Returns whether the application is running inside an iframe.
  """
  @spec within_iframe?() :: boolean()
  def within_iframe? do
    Application.fetch_env!(:livebook, :within_iframe)
  end

  @doc """
  Returns the application service name.
  """
  @spec app_service_name() :: String.t() | nil
  def app_service_name() do
    Application.fetch_env!(:livebook, :app_service_name)
  end

  @doc """
  Returns the application service url.
  """
  @spec app_service_url() :: String.t() | nil
  def app_service_url() do
    Application.fetch_env!(:livebook, :app_service_url)
  end

  @doc """
  Returns the update check URL.
  """
  @spec update_instructions_url() :: String.t() | nil
  def update_instructions_url() do
    Application.fetch_env!(:livebook, :update_instructions_url)
  end

  @doc """
  Returns the force ssl host if any.
  """
  def force_ssl_host do
    Application.fetch_env!(:livebook, :force_ssl_host)
  end

  @doc """
  Returns rewrite_on headers.
  """
  def rewrite_on do
    Application.fetch_env!(:livebook, :rewrite_on)
  end

  @doc """
  Returns the application cacertfile if any.
  """
  # TODO: Remove env var once support is added either to Erlang/OTP 28 or Elixir v1.18
  @spec cacertfile() :: String.t() | nil
  def cacertfile() do
    Application.get_env(:livebook, :cacertfile)
  end

  @feature_flags Application.compile_env(:livebook, :feature_flags)

  @doc """
  Returns the feature flag list.
  """
  @spec feature_flags() :: keyword(boolean())
  def feature_flags() do
    @feature_flags
  end

  @doc """
  Returns enabled feature flags.
  """
  @spec enabled_feature_flags() :: list()
  def enabled_feature_flags() do
    for {flag, enabled?} <- feature_flags(), enabled?, do: flag
  end

  @doc """
  Return if the feature flag is enabled.
  """
  @spec feature_flag_enabled?(atom()) :: boolean()
  def feature_flag_enabled?(key) do
    Keyword.get(@feature_flags, key, false)
  end

  @doc """
  Return list of additional allowed hyperlink schemes.
  """
  @spec allowed_uri_schemes() :: list(String.t())
  def allowed_uri_schemes() do
    Application.fetch_env!(:livebook, :allowed_uri_schemes)
  end

  @doc """
  Returns a random id set on boot.
  """
  def random_boot_id() do
    Application.fetch_env!(:livebook, :random_boot_id)
  end

  @doc """
  If we should warn when using production servers.
  """
  def warn_on_live_teams_server?() do
    Application.get_env(:livebook, :warn_on_live_teams_server, false)
  end

  @app_version Mix.Project.config()[:version]

  @doc """
  Returns the current version of running Livebook.
  """
  def app_version(), do: @app_version

  @app? Mix.target() == :app

  @doc """
  Returns whether running at the desktop app.
  """
  @spec app?() :: boolean()
  def app?(), do: @app?

  @doc """
  Returns the GitHub org/repo where the releases are created.
  """
  @spec github_release_info() :: %{repo: String.t(), version: String.t()}
  def github_release_info() do
    Application.get_env(:livebook, :github_release_info)
  end

  @doc """
  Aborts booting due to a configuration error.
  """
  @spec abort!(String.t()) :: no_return()
  def abort!(message) do
    IO.puts("\nERROR!!! [Livebook] " <> message)
    System.halt(1)
  end

  ## Parsing

  @doc """
  Parses and validates dir from env.
  """
  def writable_dir!(env) do
    if dir = System.get_env(env) do
      if writable_dir?(dir) do
        Path.expand(dir)
      else
        abort!("expected #{env} to be a writable directory: #{dir}")
      end
    end
  end

  defp writable_dir?(path) do
    case File.stat(path) do
      {:ok, %{type: :directory, access: access}} when access in [:read_write, :write] -> true
      _ -> false
    end
  end

  @doc """
  Parses and validates the secret from env.
  """
  def secret!(env) do
    if secret_key_base = System.get_env(env) do
      if byte_size(secret_key_base) < 64 do
        abort!(
          "cannot start Livebook because #{env} must be at least 64 characters. " <>
            "Invoke `openssl rand -base64 48` to generate an appropriately long secret."
        )
      end

      secret_key_base
    end
  end

  @doc """
  Parses and validates log level from env.
  """
  def log_level!(env) do
    levels = ~w(error warning notice info debug)

    if level = System.get_env(env) do
      if level in levels do
        String.to_atom(level)
      else
        abort!("expected #{env} to be one of #{Enum.join(levels, ", ")}, got: #{inspect(levels)}")
      end
    end
  end

  @doc """
  Parses and validates log metadata keys from env.
  """
  def log_metadata!(env) do
    if metadata = System.get_env(env) do
      for item <- String.split(metadata, ","),
          key = String.trim(item),
          do: String.to_atom(key)
    end
  end

  @doc """
  Parses and validates the port from env.
  """
  def port!(env) do
    if port = System.get_env(env) do
      case Integer.parse(port) do
        {port, ""} when port >= 0 -> port
        :error -> abort!("expected #{env} to be a non-negative integer, got: #{inspect(port)}")
      end
    end
  end

  @doc """
  Parses and validates the base url path from env.
  """
  def base_url_path!(env) do
    if base_url_path = System.get_env(env) do
      String.trim_trailing(base_url_path, "/")
    end
  end

  @doc """
  Parses and validates the ip from env.
  """
  def ip!(env) do
    if ip = System.get_env(env) do
      ip!(env, ip)
    end
  end

  @doc """
  Parses and validates the ip within context.
  """
  def ip!(context, ip) do
    case ip |> String.to_charlist() |> :inet.parse_address() do
      {:ok, ip} ->
        ip

      {:error, :einval} ->
        abort!("expected #{context} to be a valid ipv4 or ipv6 address, got: #{ip}")
    end
  end

  @doc """
  Parses the cookie from env.
  """
  def cookie!(env) do
    if cookie = System.get_env(env) do
      String.to_atom(cookie)
    end
  end

  @doc """
  Parses node from env.
  """
  def node!(env) do
    if node = System.get_env(env) do
      String.to_atom(node)
    end
  end

  @doc """
  Parses info for `Plug.RewriteOn`.
  """
  def rewrite_on!(env) do
    if headers = System.get_env(env) do
      headers
      |> String.split(",")
      |> Enum.map(&(&1 |> String.trim() |> rewrite_on!(env)))
    else
      []
    end
  end

  defp rewrite_on!("x-forwarded-for", _env), do: :x_forwarded_for
  defp rewrite_on!("x-forwarded-host", _env), do: :x_forwarded_host
  defp rewrite_on!("x-forwarded-port", _env), do: :x_forwarded_port
  defp rewrite_on!("x-forwarded-proto", _env), do: :x_forwarded_proto
  defp rewrite_on!(header, env), do: abort!("unknown header #{inspect(header)} given to #{env}")

  @doc """
  Parses and validates the password from env.
  """
  def password!(env) do
    if password = System.get_env(env) do
      if byte_size(password) < 12 do
        abort!("cannot start Livebook because #{env} must be at least 12 characters")
      end

      password
    end
  end

  @doc """
  Parses boolean setting from env.
  """
  def boolean!(env, default \\ false) do
    case System.get_env(env) do
      nil -> default
      var -> var in ~w(true 1)
    end
  end

  @doc """
  Parses force ssl host setting from env.
  """
  def force_ssl_host!(env) do
    System.get_env(env)
  end

  @doc """
  Parses application cacertfile from env.
  """
  def cacertfile!(env) do
    System.get_env(env)
  end

  @doc """
  Parses application service name from env.
  """
  def app_service_name!(env) do
    System.get_env(env)
  end

  @doc """
  Parses application service url from env.
  """
  def app_service_url!(env) do
    System.get_env(env)
  end

  @doc """
  Parses update instructions url from env.
  """
  def update_instructions_url!(env) do
    System.get_env(env)
  end

  @doc """
  Parses iframe url from env.
  """
  def iframe_url!(env) do
    System.get_env(env)
  end

  @doc """
  Parses teams url from env.
  """
  def teams_url!(env) do
    System.get_env(env)
  end

  @doc """
  Parses agent name from env.
  """
  def agent_name!(env) do
    if agent_name = System.get_env(env) do
      unless agent_name =~ ~r/^[a-z0-9_\-]+$/ do
        abort!(
          "expected #{env} to consist of lowercase alphanumeric characters, dashes and underscores, got: #{agent_name}"
        )
      end

      agent_name
    end
  end

  @doc """
  Parses and validates default runtime from env.
  """
  def default_runtime!(env) do
    case System.get_env(env) do
      nil ->
        nil

      "standalone" ->
        Livebook.Runtime.Standalone.new()

      "embedded" ->
        Livebook.Runtime.Embedded.new()

      "attached:" <> config ->
        {node, cookie} = parse_connection_config!(config)
        Livebook.Runtime.Attached.new(node, cookie)

      other ->
        abort!(
          ~s{expected #{env} to be either "standalone", "attached:node:cookie" or "embedded", got: #{inspect(other)}}
        )
    end
  end

  defp parse_connection_config!(config) do
    {:ok, node, cookie} = Livebook.Utils.split_at_last_occurrence(config, ":")

    node = String.to_atom(node)
    cookie = String.to_atom(cookie)

    {node, cookie}
  end

  @doc """
  Parses and validates apps warmup mode from env.
  """
  def apps_path_warmup!(env) do
    case System.get_env(env) do
      nil ->
        nil

      "auto" ->
        :auto

      "manual" ->
        :manual

      other ->
        abort!(~s{expected #{env} to be either "auto" or "manual", got: #{inspect(other)}})
    end
  end

  @doc """
  Parses and validates allowed URI schemes from env.
  """
  def allowed_uri_schemes!(env) do
    if schemes = System.get_env(env) do
      String.split(schemes, ",", trim: true)
    end
  end

  @doc """
  Parses and validates DNS cluster query from env.
  """
  def dns_cluster_query!(env) do
    if cluster_config = System.get_env(env) do
      case cluster_config do
        "dns:" <> query ->
          query

        other ->
          abort!(~s{expected #{env} to be "dns:query", got: #{inspect(other)}})
      end
    end
  end

  @identity_providers %{
    "basic_auth" => Livebook.ZTA.BasicAuth,
    "cloudflare" => Livebook.ZTA.Cloudflare,
    "google_iap" => Livebook.ZTA.GoogleIAP,
    "tailscale" => Livebook.ZTA.Tailscale
  }

  @doc """
  Parses zero trust identity provider from env.
  """
  def identity_provider!(env) do
    case System.get_env(env) do
      nil ->
        nil

      "custom:" <> module_key ->
        destructure [module, key], String.split(module_key, ":", parts: 2)
        module = Module.concat([module])

        if Code.ensure_loaded?(module) do
          {:custom, module, key}
        else
          abort!("module given as custom identity provider in #{env} could not be found")
        end

      provider ->
        with [type, key] <- String.split(provider, ":", parts: 2),
             %{^type => module} <- @identity_providers do
          {:zta, module, key}
        else
          _ -> abort!("invalid configuration for identity provider given in #{env}")
        end
    end
  end
end
