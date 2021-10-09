defmodule LivebookWeb.SessionController do
  use LivebookWeb, :controller

  alias Livebook.{Sessions, Session, FileSystem}

  def show_image(conn, %{"id" => id, "image" => image}) do
    case Sessions.fetch_session(id) do
      {:ok, session} ->
        file = FileSystem.File.resolve(session.images_dir, image)
        serve_static(conn, file)

      :error ->
        send_resp(conn, 404, "Not found")
    end
  end

  def download_source(conn, %{"id" => id, "format" => format}) do
    case Sessions.fetch_session(id) do
      {:ok, session} ->
        notebook = Session.get_notebook(session.pid)

        send_notebook_source(conn, notebook, format)

      :error ->
        send_resp(conn, 404, "Not found")
    end
  end

  defp send_notebook_source(conn, notebook, "livemd") do
    opts = [include_outputs: conn.params["include_outputs"] == "true"]
    source = Livebook.LiveMarkdown.Export.notebook_to_markdown(notebook, opts)

    send_download(conn, {:binary, source},
      filename: "notebook.livemd",
      content_type: "text/plain"
    )
  end

  defp send_notebook_source(conn, notebook, "exs") do
    source = Livebook.Notebook.Export.Elixir.notebook_to_elixir(notebook)

    send_download(conn, {:binary, source},
      filename: "notebook.exs",
      content_type: "text/plain"
    )
  end

  defp send_notebook_source(conn, _notebook, _format) do
    send_resp(conn, 400, "Invalid format, supported formats: livemd, exs")
  end

  defp serve_static(conn, file) do
    with {:ok, cache_state, conn} <- put_cache_header(conn, file),
         {:ok, conn} <- serve_with_cache(conn, file, cache_state) do
      conn
    else
      {:error, message} -> send_resp(conn, 404, Livebook.Utils.upcase_first(message))
    end
  end

  defp put_cache_header(conn, file) do
    with {:ok, etag} <- FileSystem.File.etag_for(file) do
      conn =
        conn
        |> put_resp_header("cache-control", "public")
        |> put_resp_header("etag", etag)

      if etag in get_req_header(conn, "if-none-match") do
        {:ok, :fresh, conn}
      else
        {:ok, :stale, conn}
      end
    end
  end

  defp serve_with_cache(conn, file, :stale) do
    filename = FileSystem.File.name(file)
    content_type = MIME.from_path(filename)

    with {:ok, content} <- FileSystem.File.read(file) do
      conn
      |> put_resp_header("content-type", content_type)
      |> send_resp(200, content)
      |> then(&{:ok, &1})
    end
  end

  defp serve_with_cache(conn, _file, :fresh) do
    {:ok, send_resp(conn, 304, "")}
  end
end
