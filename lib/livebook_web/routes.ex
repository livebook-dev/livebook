defmodule LivebookWeb.Routes do

  def main_paths() do
    quote do
      live "/", HomeLive, :page
      live "/home/user-profile", HomeLive, :user
      live "/home/import/:tab", HomeLive, :import
      live "/home/sessions/:session_id/close", HomeLive, :close_session
      live "/explore", ExploreLive, :page
      live "/explore/user-profile", ExploreLive, :user
      live "/explore/notebooks/:slug", ExploreLive, :notebook
      live "/sessions/:id", SessionLive, :page
      live "/sessions/:id/user-profile", SessionLive, :user
      live "/sessions/:id/shortcuts", SessionLive, :shortcuts
      live "/sessions/:id/settings/runtime", SessionLive, :runtime_settings
      live "/sessions/:id/settings/file", SessionLive, :file_settings
      live "/sessions/:id/bin", SessionLive, :bin
      live "/sessions/:id/cell-settings/:cell_id", SessionLive, :cell_settings
      live "/sessions/:id/cell-upload/:cell_id", SessionLive, :cell_upload
      live "/sessions/:id/delete-section/:section_id", SessionLive, :delete_section
      get "/sessions/:id/images/:image", SessionController, :show_image
    end
  end

  def live_dashboard_path() do
    quote do
      live_dashboard "/dashboard",
        metrics: LivebookWeb.Telemetry,
        home_app: {"Livebook", :livebook}
    end
  end

  def authenticate_paths() do
    quote do
      get "/", AuthController, :index
      post "/", AuthController, :authenticate
    end
  end

  defmacro __using__(_) do

    quote do

      scope unquote(Livebook.Config.base_url_path), LivebookWeb do
        pipe_through [:browser, :auth]

        unquote(main_paths())
        unquote(live_dashboard_path())
      end

      scope unquote(Livebook.Config.base_url_path)<>"/authenticate", LivebookWeb do
        pipe_through :browser

        unquote(authenticate_paths())
      end

    end
  end
end
