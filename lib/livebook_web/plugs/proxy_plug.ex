defmodule LivebookWeb.ProxyPlug do
  @behaviour Plug
  import Plug.Conn

  alias Livebook.{App, Apps, Session, Sessions}
  alias LivebookWeb.NotFoundError

  @impl true
  def init(opts), do: opts

  @impl true
  def call(%{path_info: ["sessions", id, "proxy" | path_info]} = conn, _opts) do
    session = fetch_session!(id)
    pid = fetch_kino_proxy!(session)
    conn = prepare_conn(conn, path_info, ["sessions", id, "proxy"])
    {conn, _} = Kino.Proxy.serve(pid, conn)

    halt(conn)
  end

  def call(%{path_info: ["apps", slug, "proxy" | path_info]} = conn, _opts) do
    app = fetch_app!(slug)
    id = App.get_session_id(app.pid)
    session = fetch_session!(id)
    pid = fetch_kino_proxy!(session)
    conn = prepare_conn(conn, path_info, ["apps", slug, "proxy"])
    {conn, _} = Kino.Proxy.serve(pid, conn)

    halt(conn)
  end

  def call(conn, _opts) do
    conn
  end

  defp fetch_app!(slug) do
    case Apps.fetch_app(slug) do
      {:ok, app} -> app
      :error -> raise NotFoundError, "could not find an app matching #{inspect(slug)}"
    end
  end

  defp fetch_session!(id) do
    case Sessions.fetch_session(id) do
      {:ok, session} -> session
      {:error, _} -> raise NotFoundError, "could not find a session matching #{id}"
    end
  end

  defp fetch_kino_proxy!(session) do
    case Session.fetch_kino_proxy(session.pid) do
      {:ok, pid} -> pid
      {:error, _} -> raise NotFoundError, "could not find a kino proxy running"
    end
  end

  defp prepare_conn(conn, path_info, script_name) do
    %{conn | path_info: path_info, script_name: conn.script_name ++ script_name}
  end
end
