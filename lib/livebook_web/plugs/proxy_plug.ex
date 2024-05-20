defmodule LivebookWeb.ProxyPlug do
  @behaviour Plug
  import Plug.Conn

  alias LivebookWeb.NotFoundError

  @impl true
  def init(opts), do: opts

  @impl true
  def call(%{path_info: ["sessions", id, "proxy" | path_info]} = conn, _opts) do
    session = fetch_session!(id)
    pid = fetch_proxy_handler!(session)
    conn = prepare_conn(conn, path_info, ["sessions", id, "proxy"])
    {conn, _} = Kino.Proxy.serve(pid, conn)

    halt(conn)
  end

  def call(%{path_info: ["apps", slug, id, "proxy" | path_info]} = conn, _opts) do
    app = fetch_app!(slug)

    unless Enum.any?(app.sessions, &(&1.id == id)) do
      raise NotFoundError, "could not find an app session matching #{inspect(id)}"
    end

    session = fetch_session!(id)
    pid = fetch_proxy_handler!(session)
    conn = prepare_conn(conn, path_info, ["apps", slug, "proxy"])
    {conn, _} = Kino.Proxy.serve(pid, conn)

    halt(conn)
  end

  def call(conn, _opts) do
    conn
  end

  defp fetch_app!(slug) do
    case Livebook.Apps.fetch_app(slug) do
      {:ok, app} -> app
      :error -> raise NotFoundError, "could not find an app matching #{inspect(slug)}"
    end
  end

  defp fetch_session!(id) do
    case Livebook.Sessions.fetch_session(id) do
      {:ok, session} -> session
      {:error, _} -> raise NotFoundError, "could not find a session matching #{id}"
    end
  end

  defp fetch_proxy_handler!(session) do
    case Livebook.Session.fetch_proxy_handler(session.pid) do
      {:ok, pid} -> pid
      {:error, _} -> raise NotFoundError, "could not find a kino proxy running"
    end
  end

  defp prepare_conn(conn, path_info, script_name) do
    %{conn | path_info: path_info, script_name: conn.script_name ++ script_name}
  end
end
