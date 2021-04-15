defmodule LivebookWeb.InvalidTokenError do
  defexception plug_status: 401, message: "invalid token"
end

defmodule LivebookWeb.AuthPlug do
  @moduledoc false

  @behaviour Plug

  alias LivebookWeb.Helpers

  import Plug.Conn
  import Phoenix.Controller

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    case auth_mode() do
      :password ->
        password_authentication(conn)

      :token ->
        token_authentication(conn)

      :disabled ->
        conn
    end
  end

  @doc """
  Checks if given connection is already authenticated.
  """
  @spec authenticated?(Plug.Conn.t()) :: boolean()
  def authenticated?(conn, mode \\ auth_mode())

  def authenticated?(conn, mode) when mode in [:token, :password] do
    secret = prepare_secret(mode)

    key = Helpers.auth_cookie_key(conn, mode)
    conn = fetch_cookies(conn, signed: [key])
    cookie = conn.cookies[key]

    is_binary(cookie) and Plug.Crypto.secure_compare(cookie, secret)
  end

  def authenticated?(_conn, _mode) do
    true
  end

  defp password_authentication(conn) do
    if authenticated?(conn) do
      conn
    else
      conn
      |> redirect(to: "/authenticate")
      |> halt()
    end
  end

  defp token_authentication(conn) do
    token = prepare_secret(:token)
    cookie_key = Helpers.auth_cookie_key(conn, :token)
    token_param = Map.get(conn.query_params, "token")

    cond do
      is_binary(token_param) and Plug.Crypto.secure_compare(token_param, token) ->
        conn
        |> put_resp_cookie(cookie_key, token_param, Helpers.auth_cookie_opts())
        # Redirect to the same path without query params
        |> redirect(to: conn.request_path)
        |> halt()

      authenticated?(conn) ->
        conn

      true ->
        raise LivebookWeb.InvalidTokenError
    end
  end

  defp auth_mode() do
    Application.fetch_env!(:livebook, :authentication_mode)
  end

  defp prepare_secret(mode) do
    secret = Application.fetch_env!(:livebook, mode)

    case mode do
      :token -> secret
      :password -> :crypto.hash(:sha256, secret) |> Base.encode16()
    end
  end
end
