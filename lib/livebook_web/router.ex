defmodule LivebookWeb.Router do
  use LivebookWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {LivebookWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :authenticate
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", LivebookWeb do
    pipe_through :browser

    live "/", HomeLive, :page
    live "/home/sessions/:session_id/delete", HomeLive, :delete_session
    live "/sessions/:id", SessionLive, :page
    live "/sessions/:id/shortcuts", SessionLive, :shortcuts
    live "/sessions/:id/settings/:tab", SessionLive, :settings
    live "/sessions/:id/cell-settings/:cell_id", SessionLive, :cell_settings
    live "/sessions/:id/cell-upload/:cell_id", SessionLive, :cell_upload
    get "/sessions/:id/images/:image", SessionController, :show_image
  end

  def authenticate(conn, _otps) do
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
