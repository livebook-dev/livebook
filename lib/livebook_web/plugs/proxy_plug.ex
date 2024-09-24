defmodule LivebookWeb.ProxyPlug do
  @behaviour Plug

  import Plug.Conn

  alias LivebookWeb.NotFoundError

  @impl true
  def init(opts), do: opts

  # We group all routes under the /proxy namespace, so that it's easy
  # to expose them when Livebook runs behind an authentication proxy.
  # The users may want to expose them, for example, to use as webhooks

  @impl true
  def call(%{path_info: ["proxy", "sessions", id | path_info]} = conn, _opts) do
    session = fetch_session!(id)
    Livebook.Session.reset_auto_shutdown(session.pid)
    proxy_handler_spec = fetch_proxy_handler_spec!(session)
    conn = prepare_conn(conn, path_info, ["proxy", "sessions", id])
    call_proxy_handler(proxy_handler_spec, conn)
  end

  def call(%{path_info: ["proxy", "apps", slug, "sessions", id | path_info]} = conn, _opts) do
    app = fetch_app!(slug)

    unless Enum.any?(app.sessions, &(&1.id == id)) do
      raise NotFoundError, "could not find an app session with id #{inspect(id)}"
    end

    session = fetch_session!(id)
    Livebook.Session.reset_auto_shutdown(session.pid)
    await_app_session_ready(app, session.id)
    proxy_handler_spec = fetch_proxy_handler_spec!(session)
    conn = prepare_conn(conn, path_info, ["proxy", "apps", slug, "sessions", id])
    call_proxy_handler(proxy_handler_spec, conn)
  end

  def call(%{path_info: ["proxy", "apps", slug | path_info]} = conn, _opts) do
    app = fetch_app!(slug)

    if app.multi_session do
      raise LivebookWeb.BadRequestError,
            "the requested app is multi-session. In order to send requests to this app," <>
              " you need to start a session and use its specific URL"
    end

    session_id = Livebook.App.get_session_id(app.pid)
    {:ok, session} = Livebook.Sessions.fetch_session(session_id)
    Livebook.Session.reset_auto_shutdown(session.pid)
    await_app_session_ready(app, session.id)
    proxy_handler_spec = fetch_proxy_handler_spec!(session)
    conn = prepare_conn(conn, path_info, ["proxy", "apps", slug])
    call_proxy_handler(proxy_handler_spec, conn)
  end

  def call(conn, _opts) do
    conn
  end

  defp fetch_app!(slug) do
    case Livebook.Apps.fetch_app(slug) do
      {:ok, app} -> app
      :error -> raise NotFoundError, "could not find an app with slug #{inspect(slug)}"
    end
  end

  defp fetch_session!(id) do
    case Livebook.Sessions.fetch_session(id) do
      {:ok, session} -> session
      {:error, _} -> raise NotFoundError, "could not find a session with id #{id}"
    end
  end

  defp fetch_proxy_handler_spec!(session) do
    case Livebook.Session.fetch_proxy_handler_spec(session.pid) do
      {:ok, pid} ->
        pid

      {:error, _} ->
        raise NotFoundError,
              "the session does not listen to proxied requests." <>
                " See the Kino.Proxy documentation to learn about defining" <>
                " custom request handlers"
    end
  end

  defp prepare_conn(conn, path_info, script_name) do
    %{conn | path_info: path_info, script_name: conn.script_name ++ script_name}
  end

  defp call_proxy_handler(proxy_handler_spec, conn) do
    {module, function, args} = proxy_handler_spec
    conn = apply(module, function, args ++ [conn])
    halt(conn)
  end

  defp await_app_session_ready(app, session_id) do
    unless session_ready?(app, session_id) do
      Livebook.App.subscribe(app.slug)
      # We fetch the app again, in case it had changed before we subscribed
      app = Livebook.App.get_by_pid(app.pid)
      await_session_execution_loop(app, session_id)
      Livebook.App.unsubscribe(app.slug)
    end
  end

  defp await_session_execution_loop(%{slug: slug} = app, session_id) do
    unless session_ready?(app, session_id) do
      receive do
        {:app_updated, %{slug: ^slug} = app} ->
          await_session_execution_loop(app, session_id)
      end
    end
  end

  defp session_ready?(app, session_id) do
    if session = Enum.find(app.sessions, &(&1.id == session_id)) do
      session.app_status.execution != :executing
    else
      false
    end
  end
end
