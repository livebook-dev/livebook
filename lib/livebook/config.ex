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
      File.get_cwd!()
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
  Aborts booting due to a configuration error.
  """
  def abort!(message) do
    IO.puts("\nERROR!!! [Livebook] " <> message)
    System.halt(1)
  end
end
