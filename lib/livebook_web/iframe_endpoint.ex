defmodule LivebookWeb.IframeEndpoint do
  use Plug.Builder

  plug Plug.Static,
    at: "/iframe",
    from: {__MODULE__, :static_from, []},
    gzip: true,
    # Iframes are versioned, so we cache them for long
    cache_control_for_etags: "public, max-age=31536000",
    headers: [
      # Enable CORS to allow Livebook fetch the content and verify its integrity
      {"access-control-allow-origin", "*"},
      {"content-type", "text/html; charset=utf-8"}
    ]

  @doc false
  def static_from(), do: Path.join(Livebook.Config.priv_path(), "iframe_static")

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
      0 ->
        try do
          ThousandIsland.listener_info(__MODULE__)
        rescue
          _ -> iframe_port
        else
          {:ok, {_ip, port}} -> port
          _ -> iframe_port
        end

      _ ->
        iframe_port
    end
  end
end
