defmodule LivebookWeb.StaticPlug.File do
  @moduledoc false

  defstruct [:content, :digest]

  @type t :: %__MODULE__{content: binary(), digest: String.t()}
end

defmodule LivebookWeb.StaticPlug.Provider do
  @moduledoc false

  @type segments :: list(String.t())
  @type compression :: :gzip | nil

  @doc """
  Returns file data for the given path (given as list of segments) and compression type.
  """
  @callback get_file(segments(), compression()) :: LivebookWeb.StaticPlug.File.t() | nil

  @doc """
  Parses static files location usually passed as the `:from` option
  when configuring provider.

  See `Plug.Static` for more details.
  """
  @spec static_path({atom(), binary()} | atom() | binary()) :: binary()
  def static_path(from)

  def static_path({app, path}) when is_atom(app) and is_binary(path) do
    Path.join(Application.app_dir(app), path)
  end

  def static_path(path) when is_binary(path), do: path
  def static_path(app) when is_atom(app), do: static_path({app, "priv/static"})
end

defmodule LivebookWeb.StaticPlug do
  @moduledoc false

  # This is a simplified version of `Plug.Static` meant
  # to serve static files using the given provider.
  #
  # ## Options
  #
  # * `:file_provider` (**required**) - a module implementing `LivebookWeb.StaticPlug.Provider`
  #   behaviour, responsible for resolving file requests
  #
  # * `:at`, `:gzip` - same as `Plug.Static`

  @behaviour Plug

  import Plug.Conn

  @allowed_methods ~w(GET HEAD)

  @impl true
  def init(opts) do
    file_provider = Keyword.fetch!(opts, :file_provider)

    %{
      file_provider: file_provider,
      at: opts |> Keyword.fetch!(:at) |> Plug.Router.Utils.split(),
      gzip?: Keyword.get(opts, :gzip, false)
    }
  end

  @impl true
  def call(
        %Plug.Conn{method: method} = conn,
        %{file_provider: file_provider, at: at, gzip?: gzip?} = options
      )
      when method in @allowed_methods do
    segments = subset(at, conn.path_info)

    case encoding_with_file(conn, file_provider, segments, gzip?) do
      {encoding, file} ->
        serve_static(conn, encoding, file, segments, options)

      :error ->
        conn
    end
  end

  def call(conn, _options) do
    conn
  end

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

  defp encoding_with_file(conn, file_provider, segments, gzip?) do
    cond do
      file = gzip? and accept_encoding?(conn, "gzip") && file_provider.get_file(segments, :gzip) ->
        {"gzip", file}

      file = file_provider.get_file(segments, nil) ->
        {nil, file}

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
