defmodule LivebookWeb.AuthPlug do
  @moduledoc false

  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  use LivebookWeb, :verified_routes

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
  @spec store(Plug.Conn.t(), Livebook.Config.auth_mode(), String.t()) :: Plug.Conn.t()
  def store(conn, mode, value) do
    conn
    |> put_session(key(conn.port, mode), hash(value))
    |> configure_session(renew: true)
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

  def authenticated?(session, port, mode) when mode in [:token, :password, :cloudflare] do
    secret = session[key(port, mode)]

    is_binary(secret) and mode == Livebook.Config.auth_mode() and
      Plug.Crypto.secure_compare(secret, expected(mode))
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

  defp authenticate(conn, :cloudflare) do
    with [token] <- get_req_header(conn, "cf-access-jwt-assertion"),
         {:ok, token} <- verify_token(token),
         {:ok, iss} <- verify_iss(token) do
      conn
      |> store(:cloudflare, iss)
      |> redirect(to: path_with_query(conn.request_path, conn.query_params))
      |> halt()
    else
      _ ->
        redirect_to_authenticate(conn)
    end
  end

  defp redirect_to_authenticate(conn) do
    conn
    |> then(fn
      %{method: "GET"} -> put_session(conn, :redirect_to, current_path(conn))
      conn -> conn
    end)
    |> redirect(to: ~p"/authenticate")
    |> halt()
  end

  defp path_with_query(path, params) when params == %{}, do: path
  defp path_with_query(path, params), do: path <> "?" <> URI.encode_query(params)

  defp key(port, mode), do: "#{port}:#{mode}"
  defp expected(mode), do: hash(Application.fetch_env!(:livebook, mode))
  defp hash(value), do: :crypto.hash(:sha256, value)

  defp verify_token(token) do
    iss = Application.fetch_env!(:livebook, :cloudflare)

    with {:ok, resp} <- Req.get("https://#{iss}.cloudflareaccess.com/cdn-cgi/access/certs"),
         {:ok, keys} <- verify_keys(resp.body) do
      Enum.find_value(keys, fn key ->
        case JOSE.JWT.verify(key, token) do
          {true, token, _s} -> {:ok, token}
          {_, _t, _s} -> nil
        end
      end)
    else
      _ -> nil
    end
  end

  defp verify_iss(%{fields: %{"iss" => "https://" <> domain}}) do
    iss = String.split(domain, ".") |> hd()
    if iss == Application.fetch_env!(:livebook, :cloudflare), do: {:ok, iss}
  end

  defp verify_keys(%{"keys" => keys}), do: {:ok, keys}
  defp verify_keys(_), do: nil
end
