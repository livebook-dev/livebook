defmodule LivebookWeb.AuthPlug do
  @moduledoc false

  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _otps) do
    configured_token = Application.get_env(:livebook, :token)

    # The user may run multiple Livebook instances on the same host
    # but different ports, so we this makes sure they don't override each other
    session_key = "#{conn.port}:token"

    if configured_token do
      cond do
        token = Map.get(conn.query_params, "token") ->
          conn
          |> verify_token(configured_token, token)
          |> put_session(session_key, token)
          # Redirect to the same path without query params
          |> redirect(to: conn.request_path)
          |> halt()

        token = get_session(conn, session_key) ->
          verify_token(conn, configured_token, token)

        true ->
          verify_token(conn, configured_token, nil)
      end
    else
      conn
    end
  end

  defp verify_token(conn, token, token), do: conn

  defp verify_token(conn, _token, _other) do
    conn
    |> send_resp(401, "Unauthorized")
    |> halt()
  end
end
