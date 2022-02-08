defmodule LivebookSpaceWeb.Plug do
  use Plug.Builder

  plug Plug.Static,
    from: {:livebook_space, "priv/static/iframe"},
    at: "/iframe",
    # Iframes are versioned, so we cache them for long
    cache_control_for_etags: "public, max-age=31536000",
    headers: [
      # Enable CORS to allow Livebook fetch the content and verify its integrity
      {"access-control-allow-origin", "*"},
      {"content-type", "text/html; charset=utf-8"}
    ]

  plug Plug.Static, from: :livebook_space, at: "/"

  plug :not_found

  defp not_found(%{path_info: [], method: "GET"} = conn, _) do
    call(%{conn | path_info: ["index.html"], request_path: "/index.html"}, [])
  end

  defp not_found(conn, _) do
    send_resp(conn, 404, "not found")
  end
end
