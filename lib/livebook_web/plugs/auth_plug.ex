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
    case Application.fetch_env!(:livebook, :authentication_mode) do
      :password ->
        password = Application.fetch_env!(:livebook, :password)
        password_authentication(conn, password)

      :token ->
        token = Application.fetch_env!(:livebook, :token)
        token_authentication(conn, token)

      :disabled ->
        conn
    end
  end

  defp password_authentication(conn, password) do
    key = Helpers.auth_cookie_key(conn, :password)
    conn = fetch_cookies(conn, signed: [key])
    pass_cookie = conn.cookies[key]

    if is_binary(pass_cookie) and Plug.Crypto.secure_compare(pass_cookie, password) do
      conn
    else
      conn
      |> redirect(to: "/authenticate")
      |> halt()
    end
  end

  defp token_authentication(conn, token) do
    key = Helpers.auth_cookie_key(conn, :token)
    conn = fetch_cookies(conn, signed: [key])
    token_param = Map.get(conn.query_params, "token")
    token_cookie = conn.cookies[key]

    cond do
      is_binary(token_param) and Plug.Crypto.secure_compare(token_param, token) ->
        conn
        |> put_resp_cookie(key, token_param, Helpers.auth_cookie_opts())
        # Redirect to the same path without query params
        |> redirect(to: conn.request_path)
        |> halt()

      is_binary(token_cookie) and Plug.Crypto.secure_compare(token_cookie, token) ->
        conn

      true ->
        raise LivebookWeb.InvalidTokenError
    end
  end
end
