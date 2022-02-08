defmodule Livebook.Config do
  @moduledoc false

  alias Livebook.FileSystem

  @type auth_mode() :: :token | :password | :disabled

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
  Returns the runtime module and `init` args used to start
  the default runtime.
  """
  @spec default_runtime() :: {Livebook.Runtime.t(), list()}
  def default_runtime() do
    Application.fetch_env!(:livebook, :default_runtime)
  end

  @doc """
  Returns the authentication mode.
  """
  @spec auth_mode() :: auth_mode()
  def auth_mode() do
    Application.fetch_env!(:livebook, :authentication_mode)
  end

  @doc """
  Returns the local filesystem.
  """
  @spec local_filesystem() :: FileSystem.t()
  def local_filesystem do
    :persistent_term.get(:livebook_local_filesystem)
  end

  @doc """
  Returns the local filesystem home.
  """
  @spec local_filesystem_home() :: FileSystem.File.t()
  def local_filesystem_home do
    FileSystem.File.new(local_filesystem())
  end

  @doc """
  Returns the home path.
  """
  @spec home() :: String.t()
  def home do
    Application.get_env(:livebook, :home) || System.user_home() || File.cwd!()
  end

  @doc """
  Returns the configuration path.
  """
  @spec data_path() :: String.t()
  def data_path() do
    Application.get_env(:livebook, :data_path) || :filename.basedir(:user_data, "livebook")
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
  def token_enabled!(env) do
    System.get_env(env, "1") in ~w(true 1)
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
        {Livebook.Runtime.ElixirStandalone, []}

      "embedded" ->
        {Livebook.Runtime.Embedded, []}

      "mix" ->
        case mix_path(File.cwd!()) do
          {:ok, path} ->
            {Livebook.Runtime.MixStandalone, [path]}

          :error ->
            abort!(
              "the current directory is not a Mix project, make sure to specify the path explicitly with mix:path"
            )
        end

      "mix:" <> path ->
        case mix_path(path) do
          {:ok, path} ->
            {Livebook.Runtime.MixStandalone, [path]}

          :error ->
            abort!(~s{"#{path}" does not point to a Mix project})
        end

      "attached:" <> config ->
        {node, cookie} = parse_connection_config!(config)
        {Livebook.Runtime.Attached, [node, cookie]}

      other ->
        abort!(
          ~s{expected #{context} to be either "standalone", "mix[:path]" or "embedded", got: #{inspect(other)}}
        )
    end
  end

  defp mix_path(path) do
    path = Path.expand(path)
    mixfile = Path.join(path, "mix.exs")

    if File.exists?(mixfile) do
      {:ok, path}
    else
      :error
    end
  end

  defp parse_connection_config!(config) do
    {node, cookie} = split_at_last_occurrence(config, ":")

    unless node =~ "@" do
      abort!(~s{expected node to include hostname, got: #{inspect(node)}})
    end

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
end
