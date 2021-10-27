defmodule LivebookWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :livebook

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  @session_options [
    store: :cookie,
    key: "_livebook_key",
    signing_salt: "deadbook"
  ]

  socket "/live", Phoenix.LiveView.Socket,
    # Don't check the origin as we don't know how the web app is gonna be accessed.
    # It runs locally, but may be exposed via IP or domain name.
    # The WebSocket connection is already protected from CSWSH by using CSRF token.
    websocket: [check_origin: false, connect_info: [:user_agent, session: @session_options]]

  # We use Escript for distributing Livebook, so we don't have access to the static
  # files at runtime in the prod environment. To overcome this we load contents of
  # those files at compilation time, so that they become a part of the executable
  # and can be served from memory.
  defmodule AssetsMemoryProvider do
    use LivebookWeb.MemoryProvider,
      from: Path.expand("../../static", __DIR__),
      gzip: true
  end

  defmodule AssetsFileSystemProvider do
    use LivebookWeb.FileSystemProvider,
      from: "tmp/static_dev"
  end

  # Serve static files at "/"

  if code_reloading? do
    # In development we use assets from tmp/static_dev (rebuilt dynamically on every change).
    # Note that this directory doesn't contain predefined files (e.g. images), so we also
    # use `AssetsMemoryProvider` to serve those from static/.
    plug LivebookWeb.StaticPlug,
      at: "/",
      file_provider: AssetsFileSystemProvider,
      gzip: false
  end

  plug LivebookWeb.StaticPlug,
    at: "/",
    file_provider: AssetsMemoryProvider,
    gzip: true

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options

  # Run custom plugs from the app configuration
  plug LivebookWeb.ConfiguredPlug

  plug LivebookWeb.Router

  def access_url() do
    root_url = url()

    if Livebook.Config.auth_mode() == :token do
      token = Application.fetch_env!(:livebook, :token)
      root_url <> "/?token=" <> token
    else
      root_url
    end
  end
end
