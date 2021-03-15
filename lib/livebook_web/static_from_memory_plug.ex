defmodule LivebookWeb.StaticFromMemoryPlug.File do
  @moduledoc false

  defstruct [:content, :digest]

  @type t :: %__MODULE__{content: binary(), digest: String.t()}
end

defmodule LivebookWeb.StaticFromMemoryPlug do
  @moduledoc false

  # This is a simplified version of `Plug.Static` meant
  # to serve static files from memory loaded with `preload_files!`.

  @behaviour Plug

  import Plug.Conn
  alias LivebookWeb.StaticFromMemoryPlug

  @allowed_methods ~w(GET HEAD)

  @doc """
  Loads all necessary file data and contents.

  Should be used at compile time and passed as a plug option,
  so that all the file contents are a part of the compiled module.

  Only non-digest versions of the static files are loaded,
  and if there's a compressed version of the given file it is loaded instead.
  The `cache_manifest.json` must be present in `priv/static` (generated with `mix phx.digest`).

  ## Options

  * `:from` (**required**) - the file system path to read static assets from.
    It can be either: a string containing a file system path, an atom representing
    the application name (where assets will be served from `priv/static`),
    or a tuple containing the application name and the directory to serve assets from (besides `priv/static`).

  * `:only` - filters which files to load. For every static file path
    we match if its first part matches one of the filters.
  """
  @spec preload_files!(keyword()) :: %{String.t() => StaticFromMemoryPlug.File.t()}
  def preload_files!(opts) do
    from =
      case Keyword.fetch!(opts, :from) do
        {_, _} = from -> from
        from when is_atom(from) -> {from, "priv/static"}
        from when is_binary(from) -> from
        _ -> raise ArgumentError, ":from must be an atom, a binary or a tuple"
      end

    only = Keyword.get(opts, :only, nil)

    static_path = from_path(from)

    paths = static_file_paths!(static_path)
    paths = Enum.filter(paths, &allowed?(only, &1))

    paths = Enum.map(paths, fn file ->
      abs_path = Path.join(static_path, file)

      # If there's a gzipped version, don't include the uncompressed one,
      # as we have to load all the contents into the memory.
      if File.exists?(abs_path <> ".gz") do
        file <> ".gz"
      else
        file
      end
    end)

    Map.new(paths, fn path ->
      abs_path = Path.join(static_path, path)
      content = File.read!(abs_path)
      digest = content |> :erlang.md5() |> Base.encode16(case: :lower)
      {path, %StaticFromMemoryPlug.File{content: content, digest: digest}}
    end)
  end

  defp from_path({app, from}) when is_atom(app) and is_binary(from),
    do: Path.join(Application.app_dir(app), from)

  defp from_path(from), do: from

  defp static_file_paths!(static_path) do
    cache_manifest_path = Path.join(static_path, "cache_manifest.json")
    %{"latest" => latest} = cache_manifest_path |> File.read!() |> Jason.decode!()
    Map.keys(latest)
  end

  defp allowed?(nil, _path), do: true

  defp allowed?(only, path) do
    [part | _] = Path.split(path)
    part in only
  end

  # Callbacks

  @impl true
  def init(opts) do
    files = Keyword.fetch!(opts, :files)

    %{
      files: files,
      at: opts |> Keyword.fetch!(:at) |> Plug.Router.Utils.split(),
      gzip?: Keyword.get(opts, :gzip, false)
    }
  end

  @impl true
  def call(%Plug.Conn{method: meth} = conn, %{files: files, at: at, gzip?: gzip?} = options)
      when meth in @allowed_methods do
    segments = subset(at, conn.path_info)
    path = segments_to_path(segments)

    case encoding_with_file(conn, files, path, gzip?) do
      {encoding, file} ->
        serve_static(conn, encoding, file, segments, options)

      :error ->
        conn
    end
  end

  def call(conn, _options) do
    conn
  end

  defp segments_to_path([]), do: ""
  defp segments_to_path(segments), do: Path.join(segments)

  defp serve_static(conn, content_encoding, file, segments, options) do
    case put_cache_header(conn, file) do
      {:stale, conn} ->
        filename = List.last(segments)
        content_type = MIME.from_path(filename)

        conn
        |> put_resp_header("content-type", content_type)
        |> maybe_add_encoding(content_encoding)
        |> maybe_add_vary(options)
        |> send_resp(200, file.content)
        |> halt()

      {:fresh, conn} ->
        conn
        |> maybe_add_vary(options)
        |> send_resp(304, "")
        |> halt()
    end
  end

  defp maybe_add_encoding(conn, nil), do: conn
  defp maybe_add_encoding(conn, ce), do: put_resp_header(conn, "content-encoding", ce)

  # If we serve gzip at any moment, we need to set the proper vary
  # header regardless of whether we are serving gzip content right now.
  # See: http://www.fastly.com/blog/best-practices-for-using-the-vary-header/
  defp maybe_add_vary(conn, %{gzip?: true}) do
    update_in(conn.resp_headers, &[{"vary", "Accept-Encoding"} | &1])
  end

  defp maybe_add_vary(conn, _options), do: conn

  defp put_cache_header(conn, file) do
    etag = etag_for_file(file)

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

  defp etag_for_file(file) do
    <<?", file.digest::binary, ?">>
  end

  defp encoding_with_file(conn, files, path, gzip?) do
    cond do
      gzip? and accept_encoding?(conn, "gzip") && Map.has_key?(files, path <> ".gz") ->
        {"gzip", Map.fetch!(files, path <> ".gz")}

      Map.has_key?(files, path) ->
        {nil, Map.fetch!(files, path)}

      true ->
        :error
    end
  end

  defp accept_encoding?(conn, encoding) do
    encoding? = &String.contains?(&1, [encoding, "*"])

    Enum.any?(get_req_header(conn, "accept-encoding"), fn accept ->
      accept |> Plug.Conn.Utils.list() |> Enum.any?(encoding?)
    end)
  end

  defp subset([h | expected], [h | actual]), do: subset(expected, actual)
  defp subset([], actual), do: actual
  defp subset(_, _), do: []
end
