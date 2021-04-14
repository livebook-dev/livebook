defmodule LivebookWeb.InvalidTokenError do
  defexception plug_status: 401, message: "invalid token"
end

defmodule LivebookWeb.AuthPlug do
  @moduledoc false

  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  @cookie_opts [sign: true, max_age: 2_592_000]

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, opts) do
    cond do
      Application.fetch_env!(:livebook, :password_authentication) == true ->
        password = Application.fetch_env!(:livebook, :password)
        secret_authentication(conn, password, :password, opts)

      Application.fetch_env!(:livebook, :token_authentication) == true ->
        token = Application.fetch_env!(:livebook, :token)
        secret_authentication(conn, token, :token, opts)

      true ->
        conn
    end
  end

  defp secret_authentication(conn, secret, type, opts) do
    # The user may run multiple Livebook instances on the same host
    # on different ports, so we scope the cookie name under port
    secret_cookie = "#{conn.port}:#{inspect(type)}"

    conn = fetch_cookies(conn, signed: [secret_cookie])
    param_secret = get_param_secret(conn, type)
    cookie_secret = conn.cookies[secret_cookie]

    cond do
      # check fresh secret
      is_binary(param_secret) and Plug.Crypto.secure_compare(param_secret, secret) ->
        conn
        |> put_resp_cookie(secret_cookie, param_secret, @cookie_opts)
        # Redirect to the same path without query params
        |> redirect(to: opts[:path] || conn.request_path)
        |> halt()

      # check secret stored in cookie
      is_binary(cookie_secret) and Plug.Crypto.secure_compare(cookie_secret, secret) ->
        conn

      # on password type redirect to proper route
      type == :password ->
        conn |> redirect(to: "/authorize") |> halt()

      # on token type let it crash so phoenix can show the error page
      true ->
        raise LivebookWeb.InvalidTokenError
    end
  end

  defp get_param_secret(conn, :token) do
    Map.get(conn.query_params, "secret")
  end

  defp get_param_secret(conn, :password) do
    case conn.params do
      %{"secret" => pass} -> pass
      _ -> nil
    end
  end
end
