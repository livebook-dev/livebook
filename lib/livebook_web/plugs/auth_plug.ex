defmodule LivebookWeb.InvalidTokenError do
  defexception plug_status: 401, message: "invalid token"
end

defmodule LivebookWeb.AuthPlug do
  @moduledoc false

  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    mode = Livebook.Config.auth_mode()

    if authenticated?(conn, mode) do
      conn
    else
      authenticate(conn, mode)
    end
  end

  @doc """
  Stores in the session the secret for the given mode.
  """
  def store(conn, mode, value) do
    put_session(conn, key(conn, mode), hash(value))
  end

  @doc """
  Checks if given connection is already authenticated.
  """
  @spec authenticated?(Plug.Conn.t(), Livebook.Config.auth_mode()) :: boolean()
  def authenticated?(conn, mode)

  def authenticated?(conn, mode) when mode in [:token, :password] do
    secret = get_session(conn, key(conn, mode))
    is_binary(secret) and Plug.Crypto.secure_compare(secret, expected(mode))
  end

  def authenticated?(_conn, _mode) do
    true
  end

  defp authenticate(conn, :password) do
    conn
    |> redirect(to: "/authenticate")
    |> halt()
  end

  defp authenticate(conn, :token) do
    token = Map.get(conn.query_params, "token")

    if is_binary(token) and Plug.Crypto.secure_compare(hash(token), expected(:token)) do
      # Redirect to the same path without query params
      conn
      |> store(:token, token)
      |> redirect(to: conn.request_path)
      |> halt()
    else
      raise LivebookWeb.InvalidTokenError
    end
  end

  defp key(conn, mode), do: "#{conn.port}:#{mode}"
  defp expected(mode), do: hash(Application.fetch_env!(:livebook, mode))
  defp hash(value), do: :crypto.hash(:sha256, value)
end
