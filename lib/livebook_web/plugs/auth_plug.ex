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
  def call(conn, _otps) do
    case Application.get_env(:livebook, :token) do
      nil -> conn
      token -> token_authentication(conn, token)
    end
  end

  defp token_authentication(conn, token) do
    # The user may run multiple Livebook instances on the same host
    # on different ports, so we scope the cookie name under port
    token_cookie = "#{conn.port}:token"

    conn = fetch_cookies(conn, signed: [token_cookie])

    param_token = Map.get(conn.query_params, "token")
    cookie_token = conn.cookies[token_cookie]

    cond do
      is_binary(param_token) and Plug.Crypto.secure_compare(param_token, token) ->
        conn
        |> put_resp_cookie(token_cookie, param_token, @cookie_opts)
        # Redirect to the same path without query params
        |> redirect(to: conn.request_path)
        |> halt()

      is_binary(cookie_token) and Plug.Crypto.secure_compare(cookie_token, token) ->
        conn

      true ->
        raise LivebookWeb.InvalidTokenError
    end
  end
end
