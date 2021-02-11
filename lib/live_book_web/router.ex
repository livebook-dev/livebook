defmodule LiveBookWeb.Router do
  use LiveBookWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {LiveBookWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", LiveBookWeb do
    pipe_through :browser

    live "/", HomeLive, :page
    live "/sessions", SessionsLive, :page
    live "/sessions/:id", SessionLive, :page
    live "/sessions/:id/runtime", SessionLive, :runtime
  end

  # Other scopes may use custom stacks.
  # scope "/api", LiveBookWeb do
  #   pipe_through :api
  # end
end
