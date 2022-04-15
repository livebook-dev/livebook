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
    put_session(conn, key(conn.port, mode), hash(value))
  end

  @doc """
  Checks if given connection is already authenticated.
  """
  @spec authenticated?(Plug.Conn.t(), Livebook.Config.auth_mode()) :: boolean()
  def authenticated?(conn, mode) do
    authenticated?(get_session(conn), conn.port, mode)
  end

  @doc """
  Checks if the given session is authenticated.
  """
  @spec authenticated?(map(), non_neg_integer(), Livebook.Config.auth_mode()) :: boolean()
  def authenticated?(session, port, mode)

  def authenticated?(_session, _port, :disabled) do
    true
  end

  def authenticated?(session, port, mode) when mode in [:token, :password] do
    secret = session[key(port, mode)]
    is_binary(secret) and Plug.Crypto.secure_compare(secret, expected(mode))
  end

  defp authenticate(conn, :password) do
    redirect_to_authenticate(conn)
  end

  defp authenticate(conn, :token) do
    {token, query_params} = Map.pop(conn.query_params, "token")

    if is_binary(token) and Plug.Crypto.secure_compare(hash(token), expected(:token)) do
      # Redirect to the same path without query params
      conn
      |> store(:token, token)
      |> redirect(to: path_with_query(conn.request_path, query_params))
      |> halt()
    else
      redirect_to_authenticate(conn)
    end
  end

  defp redirect_to_authenticate(conn) do
    conn
    |> then(fn
      %{method: "GET"} -> put_session(conn, :redirect_to, current_path(conn))
      conn -> conn
    end)
    |> redirect(to: "/authenticate")
    |> halt()
  end

  defp path_with_query(path, params) when params == %{}, do: path
  defp path_with_query(path, params), do: path <> "?" <> URI.encode_query(params)

  defp key(port, mode), do: "#{port}:#{mode}"
  defp expected(mode), do: hash(Application.fetch_env!(:livebook, mode))
  defp hash(value), do: :crypto.hash(:sha256, value)
end
