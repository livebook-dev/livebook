defmodule LivebookWeb.ProxyPlug do
  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  alias Livebook.{App, Apps, Session, Sessions}

  @impl true
  def init(opts), do: opts

  @impl true
  def call(%{params: %{"proxied_path" => path}} = conn, _opts) when is_list(path) do
    with {:ok, session} <- fetch_notebook_session(conn),
         {:ok, pid} <- Session.fetch_kino_proxy(session.pid) do
      {conn, _} = Kino.Proxy.run(pid, conn)
      conn
    else
      {:error, reason} -> render_error(conn, reason)
    end
  end

  def call(conn, _opts) do
    conn
  end

  defp fetch_notebook_session(%{params: %{"slug" => app_slug}}) do
    case Apps.fetch_app(app_slug) do
      {:ok, app} -> app.pid |> App.get_session_id() |> Sessions.fetch_session()
      :error -> {:error, :not_found}
    end
  end

  defp fetch_notebook_session(%{params: %{"id" => session_id}}) do
    Sessions.fetch_session(session_id)
  end

  defp render_error(conn, reason) do
    conn
    |> put_reason_status(reason)
    |> put_content_type_view()
    |> render_content_type(reason)
    |> halt()
  end

  defp put_reason_status(conn, :not_found), do: put_status(conn, :not_found)
  defp put_reason_status(conn, _), do: put_status(conn, :bad_request)

  defp put_content_type_view(conn) do
    case get_req_header(conn, "content-type") do
      ["application/json"] -> put_view(conn, LivebookWeb.ErrorJSON)
      _ -> put_view(conn, LivebookWeb.ErrorHTML)
    end
  end

  defp render_content_type(conn, :not_found) do
    case get_req_header(conn, "content-type") do
      ["application/json"] -> render(conn, "404.json")
      _ -> render(conn, "404.html", %{status: 404})
    end
  end

  defp render_content_type(conn, _) do
    case get_req_header(conn, "content-type") do
      ["application/json"] -> render(conn, "400.json")
      _ -> render(conn, "400.html", %{status: 400})
    end
  end
end
