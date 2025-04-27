defmodule LivebookWeb.SessionController do
  use LivebookWeb, :controller

  alias Livebook.Sessions
  alias Livebook.Session
  alias Livebook.FileSystem
  alias LivebookWeb.CodecHelpers

  def show_file(conn, %{"id" => id, "name" => name}) do
    with {:ok, session} <- Sessions.fetch_session(id),
         {:ok, file_entry} <- fetch_file_entry(session, name),
         true <- file_entry.type == :attachment do
      file = FileSystem.File.resolve(session.files_dir, file_entry.name)
      serve_static(conn, file)
    else
      _ ->
        send_resp(conn, 404, "Not found")
    end
  end

  defp fetch_file_entry(session, name) do
    file_entries = Session.get_notebook_file_entries(session.pid)

    Enum.find_value(file_entries, :error, fn file_entry ->
      if file_entry.name == name do
        {:ok, file_entry}
      end
    end)
  end

  def download_file(conn, %{"id" => id, "name" => name}) do
    with {:ok, session} <- Sessions.fetch_session(id),
         {:ok, path} <- Session.fetch_file_entry_path(session.pid, name) do
      send_download(conn, {:file, path}, filename: name)
    else
      _ ->
        send_resp(conn, 404, "Not found")
    end
  end

  def download_source(conn, %{"id" => id, "format" => format}) do
    case Sessions.fetch_session(id) do
      {:ok, session} ->
        notebook = Session.get_notebook(session.pid)
        file_name = Session.file_name_for_download(session)

        send_notebook_source(conn, notebook, file_name, format)

      {:error, _} ->
        send_resp(conn, 404, "Not found")
    end
  end

  defp send_notebook_source(conn, notebook, file_name, "livemd" = format) do
    opts = [include_outputs: conn.params["include_outputs"] == "true"]
    {source, _warnings} = Livebook.LiveMarkdown.notebook_to_livemd(notebook, opts)

    send_download(conn, {:binary, source},
      filename: file_name <> "." <> format,
      content_type: "text/plain"
    )
  end

  defp send_notebook_source(conn, notebook, file_name, "exs" = format) do
    source = Livebook.Notebook.Export.Elixir.notebook_to_elixir(notebook)

    send_download(conn, {:binary, source},
      filename: file_name <> "." <> format,
      content_type: "text/plain"
    )
  end

  defp send_notebook_source(conn, _notebook, _file_name, _format) do
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

    with {:ok, content} <- FileSystem.File.read(file) do
      conn
      |> put_content_type(filename)
      |> send_resp(200, content)
      |> then(&{:ok, &1})
    end
  end

  defp serve_with_cache(conn, _file, :fresh) do
    {:ok, send_resp(conn, 304, "")}
  end

  def show_asset(conn, %{"id" => id, "hash" => hash, "file_parts" => file_parts}) do
    asset_path = Path.join(file_parts)

    # The request comes from a cross-origin iframe
    conn = allow_cors(conn)

    # This route include session id, but we want the browser to cache
    # assets across sessions, so we only ensure the asset is available
    # on this node and redirect to the corresponding route with node
    # id, rather than session id
    if ensure_asset?(id, hash, asset_path) do
      node_id = Livebook.Utils.node_id()

      conn
      |> cache_permanently()
      |> put_status(:moved_permanently)
      |> redirect(to: ~p"/public/sessions/node/#{node_id}/assets/#{hash}/#{file_parts}")
    else
      send_resp(conn, 404, "Not found")
    end
  end

  def show_cached_asset(conn, %{"node_id" => node_id, "hash" => hash, "file_parts" => file_parts}) do
    asset_path = Path.join(file_parts)

    # The request comes from a cross-origin iframe
    conn = allow_cors(conn)

    gzip_result =
      if accept_encoding?(conn, "gzip") do
        with {:ok, local_asset_path} <-
               lookup_asset_or_transfer(hash, asset_path <> ".gz", node_id) do
          conn =
            conn
            |> put_resp_header("content-encoding", "gzip")
            |> put_resp_header("vary", "Accept-Encoding")

          {:ok, local_asset_path, conn}
        end
      else
        :error
      end

    result =
      with :error <- gzip_result do
        with {:ok, local_asset_path} <- lookup_asset_or_transfer(hash, asset_path, node_id) do
          {:ok, local_asset_path, conn}
        end
      end

    case result do
      {:ok, local_asset_path, conn} ->
        conn
        |> put_content_type(asset_path)
        |> cache_permanently()
        |> send_file(200, local_asset_path)

      :error ->
        send_resp(conn, 404, "Not found")
    end
  end

  def show_input_audio(conn, %{"token" => token}) do
    {live_view_pid, input_id} = LivebookWeb.SessionHelpers.verify_input_token!(token)

    case GenServer.call(live_view_pid, {:get_input_value, input_id}) do
      {:ok, session_id, value} ->
        path = Livebook.Session.registered_file_path(session_id, value.file_ref)

        conn =
          conn
          |> cache_permanently()
          |> put_resp_header("accept-ranges", "bytes")

        case value.format do
          :pcm_f32 ->
            %{size: file_size} = File.stat!(path)

            total_size = CodecHelpers.pcm_as_wav_size(file_size)

            case parse_byte_range(conn, total_size) do
              {range_start, range_end} when range_start > 0 or range_end < total_size - 1 ->
                stream =
                  CodecHelpers.encode_pcm_as_wav_stream!(
                    path,
                    file_size,
                    value.num_channels,
                    value.sampling_rate,
                    range_start,
                    range_end - range_start + 1
                  )

                conn
                |> put_content_range(range_start, range_end, total_size)
                |> send_stream(206, stream)

              _ ->
                stream =
                  CodecHelpers.encode_pcm_as_wav_stream!(
                    path,
                    file_size,
                    value.num_channels,
                    value.sampling_rate,
                    0,
                    total_size
                  )

                conn
                |> put_resp_header("content-length", Integer.to_string(total_size))
                |> send_stream(200, stream)
            end

          :wav ->
            %{size: total_size} = File.stat!(path)

            case parse_byte_range(conn, total_size) do
              {range_start, range_end} when range_start > 0 or range_end < total_size - 1 ->
                conn
                |> put_content_range(range_start, range_end, total_size)
                |> send_file(206, path, range_start, range_end - range_start + 1)

              _ ->
                send_file(conn, 200, path)
            end
        end

      :error ->
        send_resp(conn, 404, "Not found")
    end
  end

  def show_input_image(conn, %{"token" => token}) do
    {live_view_pid, input_id} = LivebookWeb.SessionHelpers.verify_input_token!(token)

    case GenServer.call(live_view_pid, {:get_input_value, input_id}) do
      {:ok, session_id, value} ->
        path = Livebook.Session.registered_file_path(session_id, value.file_ref)

        conn
        |> cache_permanently()
        |> send_file(200, path)

      :error ->
        send_resp(conn, 404, "Not found")
    end
  end

  defp accept_encoding?(conn, encoding) do
    encoding? = &String.contains?(&1, [encoding, "*"])

    Enum.any?(get_req_header(conn, "accept-encoding"), fn accept ->
      accept |> Plug.Conn.Utils.list() |> Enum.any?(encoding?)
    end)
  end

  defp ensure_asset?(session_id, hash, asset_path) do
    case lookup_asset(hash, asset_path) do
      {:ok, _local_asset_path} ->
        true

      :error ->
        with {:ok, session} <- Sessions.fetch_session(session_id),
             :ok <- Session.fetch_assets(session.pid, hash) do
          true
        else
          _ -> false
        end
    end
  end

  @doc false
  def lookup_asset(hash, asset_path) do
    with {:ok, local_asset_path} <- Session.local_asset_path(hash, asset_path),
         true <- File.exists?(local_asset_path) do
      {:ok, local_asset_path}
    else
      _ -> :error
    end
  end

  defp lookup_asset_or_transfer(hash, asset_path, node_id) do
    with :error <- lookup_asset(hash, asset_path),
         {:ok, node} <- Livebook.Utils.node_from_id(node_id),
         {:ok, remote_asset_path} <-
           :erpc.call(node, __MODULE__, :lookup_asset, [hash, asset_path]),
         {:ok, local_asset_path} <- Session.local_asset_path(hash, asset_path) do
      transfer_file!(node, remote_asset_path, local_asset_path)
      {:ok, local_asset_path}
    end
  end

  defp transfer_file!(remote_node, remote_path, local_path) do
    File.mkdir_p!(Path.dirname(local_path))
    remote_stream = :erpc.call(remote_node, File, :stream!, [remote_path, 64_000, []])
    local_stream = File.stream!(local_path)
    Enum.into(remote_stream, local_stream)
  end

  defp allow_cors(conn) do
    put_resp_header(conn, "access-control-allow-origin", "*")
  end

  defp cache_permanently(conn) do
    put_resp_header(conn, "cache-control", "public, max-age=31536000")
  end

  defp put_content_type(conn, path) do
    content_type = MIME.from_path(path)
    put_resp_header(conn, "content-type", content_type)
  end

  defp parse_byte_range(conn, total_size) do
    with [range] <- get_req_header(conn, "range"),
         %{"bytes" => bytes} <- Plug.Conn.Utils.params(range),
         {range_start, range_end} <- start_and_end(bytes, total_size) do
      {range_start, range_end}
    else
      _ -> :error
    end
  end

  defp start_and_end("-" <> rest, total_size) do
    case Integer.parse(rest) do
      {last, ""} when last > 0 and last <= total_size -> {total_size - last, total_size - 1}
      _ -> :error
    end
  end

  defp start_and_end(range, total_size) do
    case Integer.parse(range) do
      {first, "-"} when first >= 0 ->
        {first, total_size - 1}

      {first, "-" <> rest} when first >= 0 ->
        case Integer.parse(rest) do
          {last, ""} when last >= first -> {first, min(last, total_size - 1)}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp put_content_range(conn, range_start, range_end, total_size) do
    put_resp_header(conn, "content-range", "bytes #{range_start}-#{range_end}/#{total_size}")
  end

  defp send_stream(conn, status, stream) do
    conn = send_chunked(conn, status)

    Enum.reduce_while(stream, conn, fn chunk, conn ->
      case Plug.Conn.chunk(conn, chunk) do
        {:ok, conn} ->
          {:cont, conn}

        {:error, :closed} ->
          {:halt, conn}
      end
    end)
  end
end
