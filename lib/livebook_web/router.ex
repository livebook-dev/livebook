defmodule LivebookWeb.Router do
  use LivebookWeb, :router
  import Phoenix.LiveDashboard.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {LivebookWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :auth do
    plug LivebookWeb.AuthPlug
  end

  scope "/", LivebookWeb do
    pipe_through [:browser, :auth]

    live "/", HomeLive, :page
    live "/home/sessions/:session_id/close", HomeLive, :close_session
    live "/sessions/:id", SessionLive, :page
    live "/sessions/:id/shortcuts", SessionLive, :shortcuts
    live "/sessions/:id/settings/:tab", SessionLive, :settings
    live "/sessions/:id/cell-settings/:cell_id", SessionLive, :cell_settings
    live "/sessions/:id/cell-upload/:cell_id", SessionLive, :cell_upload
    get "/sessions/:id/images/:image", SessionController, :show_image

    live_dashboard "/dashboard", metrics: LivebookWeb.Telemetry
  end

  scope "/authenticate", LivebookWeb do
    pipe_through :browser

    get "/", AuthController, :index
    post "/", AuthController, :authenticate
  end
end
