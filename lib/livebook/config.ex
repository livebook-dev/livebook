defmodule Livebook.Config do
  @moduledoc false

  alias Livebook.FileSystem

  @type auth_mode() :: :token | :password | :disabled

  identity_providers = %{
    session: LivebookWeb.SessionIdentity,
    google_iap: Livebook.ZTA.GoogleIAP,
    cloudflare: Livebook.ZTA.Cloudflare
  }

  @identity_provider_type_to_module Map.new(identity_providers, fn {key, value} ->
                                      {Atom.to_string(key), value}
                                    end)

  @identity_provider_module_to_type Map.new(identity_providers, fn {key, value} ->
                                      {value, key}
                                    end)

  @doc """
  Returns the longname if the distribution mode is configured to use long names.
  """
  @spec longname() :: binary() | nil
  def longname() do
    host = Livebook.Utils.node_host()

    if host =~ "." do
      host
    end
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
  Returns the authentication mode.
  """
  @spec auth_mode() :: auth_mode()
  def auth_mode() do
    Application.fetch_env!(:livebook, :authentication_mode)
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
    Path.join(tmp_dir, "livebook")
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
  Returns the base url path for the Livebook endpoint.
  """
  @spec base_url_path() :: String.t()
  def base_url_path() do
    path = Application.get_env(:livebook, LivebookWeb.Endpoint)[:url][:path]
    String.trim_trailing(path, "/")
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
  Returns the configured URL for the Livebook Teams endpoint.
  """
  @spec teams_url() :: String.t()
  def teams_url() do
    Application.fetch_env!(:livebook, :teams_url)
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
  @spec identity_provider() :: tuple()
  def identity_provider() do
    Application.fetch_env!(:livebook, :identity_provider)
  end

  @doc """
  Returns if the identity data is readonly.
  """
  @spec identity_readonly?() :: boolean()
  def identity_readonly?() do
    not match?({LivebookWeb.SessionIdentity, _}, Livebook.Config.identity_provider())
  end

  @doc """
  Returns identity source as a friendly atom.
  """
  @spec identity_source() :: atom()
  def identity_source() do
    {module, _} = identity_provider()
    @identity_provider_module_to_type[module]
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

  ## Parsing

  @doc """
  Parses and validates dir from env.
  """
  def writable_dir!(env) do
    if dir = System.get_env(env) do
      writable_dir!(env, dir)
    end
  end

  @doc """
  Validates `dir` within context.
  """
  def writable_dir!(context, dir) do
    if writable_dir?(dir) do
      Path.expand(dir)
    else
      abort!("expected #{context} to be a writable directory: #{dir}")
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
  Parses and validates the port from env.
  """
  def port!(env) do
    if port = System.get_env(env) do
      case Integer.parse(port) do
        {port, ""} -> port
        :error -> abort!("expected #{env} to be an integer, got: #{inspect(port)}")
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
  Parses node and distribution type from env.
  """
  def node!(node_env, distribution_env) do
    case {System.get_env(node_env), System.get_env(distribution_env, "sname")} do
      {nil, _} ->
        nil

      {name, "name"} ->
        {:longnames, String.to_atom(name)}

      {sname, "sname"} ->
        {:shortnames, String.to_atom(sname)}

      {_, other} ->
        abort!(~s(#{distribution_env} must be one of "name" or "sname", got "#{other}"))
    end
  end

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
  Parses token auth setting from env.
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
  Parses and validates default runtime from env.
  """
  def default_runtime!(env) do
    if runtime = System.get_env(env) do
      default_runtime!(env, runtime)
    end
  end

  @doc """
  Parses and validates default runtime within context.
  """
  def default_runtime!(context, runtime) do
    case runtime do
      "standalone" ->
        Livebook.Runtime.ElixirStandalone.new()

      "embedded" ->
        Livebook.Runtime.Embedded.new()

      "attached:" <> config ->
        {node, cookie} = parse_connection_config!(config)
        Livebook.Runtime.Attached.new(node, cookie)

      other ->
        abort!(
          ~s{expected #{context} to be either "standalone", "attached:node:cookie" or "embedded", got: #{inspect(other)}}
        )
    end
  end

  @doc """
  Parses and validates apps warmup mode from env.
  """
  def apps_path_warmup!(env) do
    if warmup = System.get_env(env) do
      apps_path_warmup!(env, warmup)
    end
  end

  @doc """
  Parses and validates apps warmup mode within context.
  """
  def apps_path_warmup!(context, warmup) do
    case warmup do
      "auto" ->
        :auto

      "manual" ->
        :manual

      other ->
        abort!(~s{expected #{context} to be either "auto" or "manual", got: #{inspect(other)}})
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
  Returns the current version of running Livebook.
  """
  def app_version, do: Application.spec(:livebook, :vsn) |> List.to_string()

  defp parse_connection_config!(config) do
    {node, cookie} = split_at_last_occurrence(config, ":")

    node = String.to_atom(node)
    cookie = String.to_atom(cookie)

    {node, cookie}
  end

  defp split_at_last_occurrence(string, pattern) do
    {idx, 1} = string |> :binary.matches(pattern) |> List.last()

    {
      binary_part(string, 0, idx),
      binary_part(string, idx + 1, byte_size(string) - idx - 1)
    }
  end

  @doc """
  Aborts booting due to a configuration error.
  """
  @spec abort!(String.t()) :: no_return()
  def abort!(message) do
    IO.puts("\nERROR!!! [Livebook] " <> message)
    System.halt(1)
  end

  @doc """
  Parses zero trust identity provider from env.
  """
  def identity_provider!(env) do
    if provider = System.get_env(env) do
      identity_provider!(env, provider)
    end
  end

  @doc """
  Parses and validates zero trust identity provider within context.
  """
  def identity_provider!(context, provider) do
    with [type, key] <- String.split(provider, ":", parts: 2),
         {:ok, module} <- Map.fetch(@identity_provider_type_to_module, type) do
      {module, key}
    else
      _ -> abort!("invalid configuration for identity provider given in #{context}")
    end
  end
end
