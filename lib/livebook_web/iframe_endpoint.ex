defmodule LivebookWeb.IframeEndpoint do
  use Plug.Builder

  defmodule AssetsMemoryProvider do
    use LivebookWeb.MemoryProvider,
      from: Path.expand("../../iframe/priv/static/iframe", __DIR__),
      gzip: true
  end

  plug LivebookWeb.StaticPlug,
    at: "/iframe",
    file_provider: AssetsMemoryProvider,
    gzip: true,
    headers: [
      # Enable CORS to allow Livebook fetch the content and verify its integrity
      {"access-control-allow-origin", "*"},
      # Iframes are versioned, so we cache them for long
      {"cache-control", "public, max-age=31536000"},
      # Specify the charset
      {"content-type", "text/html; charset=utf-8"}
    ]

  plug :not_found

  defp not_found(conn, _) do
    send_resp(conn, 404, "not found")
  end

  @doc """
  Returns the listening port of the iframe endpoint.
  """
  @spec port() :: pos_integer()
  def port() do
    livebook_port = Livebook.Config.port()
    iframe_port = Livebook.Config.iframe_port()

    case livebook_port do
      0 -> Livebook.Utils.get_port(__MODULE__.HTTP, iframe_port)
      _ -> iframe_port
    end
  end
end
