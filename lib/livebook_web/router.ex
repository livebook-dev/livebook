defmodule LivebookWeb.Router do
  use LivebookWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {LivebookWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", LivebookWeb do
    pipe_through :browser

    live "/", HomeLive, :page
    live "/delete-session/:session_id", HomeLive, :delete_session
    live "/sessions/:id", SessionLive, :page
    live "/sessions/:id/shortcuts", SessionLive, :shortcuts
    live "/sessions/:id/settings/:tab", SessionLive, :settings
    live "/sessions/:id/cell-settings/:cell_id", SessionLive, :cell_settings
  end
end
