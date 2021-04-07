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
  def call(conn, _otps) do
    case Application.get_env(:livebook, :token) do
      nil -> conn
      token -> token_authentication(conn, token)
    end
  end

  defp token_authentication(conn, token) do
    # The user may run multiple Livebook instances on the same host
    # but different ports, so we this makes sure they don't override each other
    session_key = "#{conn.port}:token"

    cond do
      provided_token = Map.get(conn.query_params, "token") ->
        if provided_token == token do
          conn
          |> put_session(session_key, provided_token)
          # Redirect to the same path without query params
          |> redirect(to: conn.request_path)
          |> halt()
        else
          reject_token!()
        end

      provided_token = get_session(conn, session_key) ->
        if provided_token == token do
          conn
        else
          reject_token!()
        end

      true ->
        reject_token!()
    end
  end

  defp reject_token!() do
    raise LivebookWeb.InvalidTokenError
  end
end
