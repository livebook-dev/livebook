defmodule Livebook.Config do
  @moduledoc false

  @type auth_mode() :: :token | :password | :disabled

  @doc """
  Checks if the distribution mode is configured to use short names.
  """
  @spec shortnames?() :: boolean()
  def shortnames?() do
    case Application.get_env(:livebook, :node) do
      nil -> true
      {:shortnames, _name} -> true
      {:longnames, _name} -> false
    end
  end

  @doc """
  Returns the runtime module to be used by default.
  """
  @spec default_runtime() :: Livebook.Runtime
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
  Return the root path for persisting notebooks.
  """
  @spec root_path() :: binary()
  def root_path() do
    Application.fetch_env!(:livebook, :root_path)
  end

  ## Parsing

  @doc """
  Parses and validates the root path from env.
  """
  def root_path!(env) do
    if root_path = System.get_env(env) do
      root_path!("LIVEBOOK_ROOT_PATH", root_path)
    else
      File.cwd!()
    end
  end

  @doc """
  Validates `root_path` within context.
  """
  def root_path!(context, root_path) do
    if File.dir?(root_path) do
      root_path
    else
      IO.warn("ignoring #{context} because it doesn't point to a directory: #{root_path}")
      File.cwd!()
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
  Parses and validates default runtime from env.
  """
  def default_runtime!(env) do
    if runtime = System.get_env(env) do
      case runtime do
        "standalone" ->
          Livebook.Runtime.ElixirStandalone

        "embedded" ->
          Livebook.Runtime.Embedded

        other ->
          abort!(
            ~s{expected #{env} to be either "standalone" or "embedded", got: #{inspect(other)}}
          )
      end
    end
  end

  @doc """
  Aborts booting due to a configuration error.
  """
  def abort!(message) do
    IO.puts("\nERROR!!! [Livebook] " <> message)
    System.halt(1)
  end
end
