defmodule LivebookWeb.Router do
  use LivebookWeb, :router
  import Phoenix.LiveDashboard.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {LivebookWeb.Layouts, :root}
    # Because LIVEBOOK_SECRET_KEY_BASE authentication is randomly
    # generated, the odds of getting a CSRFProtection is quite high
    # and exceptions can lead to a poor user experience.
    #
    # During authentication, configure_session(renew: true) will
    # override the configure_session(ignore: true) but the session
    # will be cleared anyway. This means an attacker can authenticate
    # someone in a given Livebook instance but they wouldn't be able
    # to do anything once the authentication goes through.
    plug :protect_from_forgery, with: :clear_session
    plug :put_secure_browser_headers
    plug :within_iframe_secure_headers
  end

  pipeline :auth do
    plug LivebookWeb.AuthPlug
    plug LivebookWeb.UserPlug
  end

  pipeline :user do
    plug LivebookWeb.UserPlug
  end

  pipeline :js_view_assets do
    plug :put_secure_browser_headers
    plug :within_iframe_secure_headers
  end

  # The /public namespace includes routes with no authentication.
  # When exposing Livebook through an authentication proxy, this
  # namespace should be configured as publicly available, in order
  # for all features to work as expected.

  scope "/public", LivebookWeb do
    pipe_through :browser

    get "/health", HealthController, :index
  end

  # The following routes are public, but should be treated as opaque
  scope "/public", LivebookWeb do
    pipe_through [:js_view_assets]

    get "/sessions/assets/:hash/*file_parts", SessionController, :show_cached_asset
    get "/sessions/:id/assets/:hash/*file_parts", SessionController, :show_asset
  end

  live_session :default,
    on_mount: [LivebookWeb.AuthHook, LivebookWeb.UserHook, LivebookWeb.Confirm] do
    scope "/", LivebookWeb do
      pipe_through [:browser, :auth]

      live "/", HomeLive, :page

      live "/open/:tab", OpenLive, :page

      live "/settings", SettingsLive, :page
      live "/settings/add-file-system", SettingsLive, :add_file_system
      live "/settings/env-var/new", SettingsLive, :add_env_var
      live "/settings/env-var/edit/:env_var_id", SettingsLive, :edit_env_var

      live "/learn", LearnLive, :page
      live "/learn/notebooks/:slug", LearnLive, :notebook

      live "/apps", AppsLive, :page

      live "/hub", Hub.NewLive, :new, as: :hub
      live "/hub/:id", Hub.EditLive, :edit, as: :hub
      live "/hub/:id/env-var/new", Hub.EditLive, :add_env_var, as: :hub
      live "/hub/:id/env-var/edit/:env_var_id", Hub.EditLive, :edit_env_var, as: :hub
      live "/hub/:id/secrets/new", Hub.EditLive, :new_secret, as: :hub
      live "/hub/:id/secrets/edit/:secret_name", Hub.EditLive, :edit_secret, as: :hub

      live "/sessions/:id", SessionLive, :page
      live "/sessions/:id/shortcuts", SessionLive, :shortcuts
      live "/sessions/:id/secrets", SessionLive, :secrets
      live "/sessions/:id/settings/runtime", SessionLive, :runtime_settings
      live "/sessions/:id/settings/file", SessionLive, :file_settings
      live "/sessions/:id/settings/app", SessionLive, :app_settings
      live "/sessions/:id/add-file/:tab", SessionLive, :add_file_entry
      live "/sessions/:id/bin", SessionLive, :bin
      get "/sessions/:id/download/export/:format", SessionController, :download_source
      live "/sessions/:id/export/:tab", SessionLive, :export
      live "/sessions/:id/cell-settings/:cell_id", SessionLive, :cell_settings
      live "/sessions/:id/insert-image", SessionLive, :insert_image
      live "/sessions/:id/insert-file", SessionLive, :insert_file
      live "/sessions/:id/package-search", SessionLive, :package_search
      get "/sessions/:id/files/:name", SessionController, :show_file
      get "/sessions/:id/images/:name", SessionController, :show_image
      get "/sessions/:id/download/files/:name", SessionController, :download_file
      live "/sessions/:id/settings/custom-view", SessionLive, :custom_view_settings
      live "/sessions/:id/*path_parts", SessionLive, :catch_all
    end

    # Public authenticated URLs that people may be directed to
    scope "/", LivebookWeb do
      pipe_through [:browser, :auth]

      live "/new", HomeLive, :public_new_notebook
      live "/import", OpenLive, :public_import
      live "/open", OpenLive, :public_open
    end
  end

  live_session :apps,
    on_mount: [LivebookWeb.AppAuthHook, LivebookWeb.UserHook, LivebookWeb.Confirm] do
    scope "/", LivebookWeb do
      pipe_through [:browser, :user]

      live "/apps/:slug", AppLive, :page
      live "/apps/:slug/new", AppLive, :new_session
      live "/apps/:slug/authenticate", AppAuthLive, :page

      live "/apps/:slug/:id", AppSessionLive, :page
      live "/apps/:slug/:id/source", AppSessionLive, :source
    end
  end

  scope "/" do
    pipe_through [:browser, :auth]

    live_dashboard "/dashboard",
      metrics: LivebookWeb.Telemetry,
      home_app: {"Livebook", :livebook},
      ecto_repos: []
  end

  scope "/authenticate", LivebookWeb do
    pipe_through :browser

    get "/", AuthController, :index
    post "/", AuthController, :authenticate
  end

  defp within_iframe_secure_headers(conn, _opts) do
    if Livebook.Config.within_iframe?() do
      delete_resp_header(conn, "x-frame-options")
    else
      conn
    end
  end
end
