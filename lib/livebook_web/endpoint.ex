defmodule LivebookWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :livebook

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  @session_options [
    store: :cookie,
    key: "lb:session",
    signing_salt: "deadbook"
  ]

  # Don't check the origin as we don't know how the web app is gonna be accessed.
  # It runs locally, but may be exposed via IP or domain name. The WebSocket
  # connection is already protected from CSWSH by using CSRF token.
  @websocket_options [
    check_origin: false,
    connect_info: [:user_agent, :uri, session: @session_options]
  ]

  socket "/live", Phoenix.LiveView.Socket, websocket: @websocket_options
  socket "/socket", LivebookWeb.Socket, websocket: @websocket_options

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

  plug :force_ssl

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
  plug :session
  plug :purge_cookies

  # Run custom plugs from the app configuration
  plug LivebookWeb.ConfiguredPlug

  plug LivebookWeb.Router

  @plug_session Plug.Session.init(@session_options ++ [same_site: "Lax"])
  @plug_session_iframe Plug.Session.init(@session_options ++ [same_site: "None", secure: true])
  def session(conn, _opts) do
    if Livebook.Config.within_iframe?() do
      Plug.Session.call(conn, @plug_session_iframe)
    else
      Plug.Session.call(conn, @plug_session)
    end
  end

  @plug_ssl Plug.SSL.init(host: {Livebook.Config, :force_ssl_host, []})
  def force_ssl(conn, _opts) do
    if Livebook.Config.force_ssl_host() do
      Plug.SSL.call(conn, @plug_ssl)
    else
      conn
    end
  end

  def cookie_options() do
    if Livebook.Config.within_iframe?() do
      [same_site: "None", secure: true]
    else
      [same_site: "Lax"]
    end
  end

  # Because we run on localhost, we may accumulate
  # cookies from several other apps. Our header limit
  # is set to 32kB. Once we are 75% of said limit,
  # we clear other cookies to make sure we don't go
  # over the limit.
  def purge_cookies(conn, _opts) do
    cookie_size =
      conn
      |> Plug.Conn.get_req_header("cookie")
      |> Enum.map(&byte_size/1)
      |> Enum.sum()

    if cookie_size > 24576 do
      conn.cookies
      |> Enum.reject(fn {key, _value} -> String.starts_with?(key, "lb:") end)
      |> Enum.take(10)
      |> Enum.reduce(conn, fn {key, _value}, conn ->
        Plug.Conn.delete_resp_cookie(conn, key)
      end)
    else
      conn
    end
  end

  def access_struct_url() do
    base =
      case struct_url() do
        %URI{scheme: "https", port: 0} = uri ->
          %{uri | port: Livebook.Utils.get_port(__MODULE__.HTTPS, 433)}

        %URI{scheme: "http", port: 0} = uri ->
          %{uri | port: Livebook.Utils.get_port(__MODULE__.HTTP, 80)}

        %URI{} = uri ->
          uri
      end

    base = update_in(base.path, &(&1 || "/"))

    if Livebook.Config.auth_mode() == :token do
      token = Application.fetch_env!(:livebook, :token)
      %{base | query: "token=" <> token}
    else
      base
    end
  end

  def access_url do
    URI.to_string(access_struct_url())
  end
end
