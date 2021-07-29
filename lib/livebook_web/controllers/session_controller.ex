defmodule LivebookWeb.SessionController do
  use LivebookWeb, :controller

  alias Livebook.{SessionSupervisor, Session}

  def show_image(conn, %{"id" => id, "image" => image}) do
    with true <- SessionSupervisor.session_exists?(id),
         %{images_dir: images_dir} <- Session.get_summary(id),
         path <- Path.join(images_dir, image),
         true <- File.exists?(path) do
      serve_static(conn, path)
    else
      _ -> send_resp(conn, 404, "Not found")
    end
  end

  def download_source(conn, %{"id" => id, "format" => format}) do
    if SessionSupervisor.session_exists?(id) do
      notebook = Session.get_notebook(id)

      send_notebook_source(conn, notebook, format)
    else
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

  defp serve_static(conn, path) do
    case put_cache_header(conn, path) do
      {:stale, conn} ->
        filename = Path.basename(path)
        content_type = MIME.from_path(filename)

        conn
        |> put_resp_header("content-type", content_type)
        |> send_file(200, path)

      {:fresh, conn} ->
        send_resp(conn, 304, "")
    end
  end

  defp put_cache_header(conn, path) do
    etag = etag_for_path(path)

    conn =
      conn
      |> put_resp_header("cache-control", "public")
      |> put_resp_header("etag", etag)

    if etag in get_req_header(conn, "if-none-match") do
      {:fresh, conn}
    else
      {:stale, conn}
    end
  end

  defp etag_for_path(path) do
    %{size: size, mtime: mtime} = File.stat!(path)
    hash = {size, mtime} |> :erlang.phash2() |> Integer.to_string(16)
    <<?", hash::binary, ?">>
  end
end
