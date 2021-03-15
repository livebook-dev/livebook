defmodule LivebookWeb.StaticProvidedPlugTest do
  use ExUnit.Case, async: true
  use Plug.Test

  defmodule MyProvider do
    @behaviour LivebookWeb.StaticProvidedPlug.Provider

    @impl true
    def valid_path?(path) do
      path in ["app.js.gz", "app.js", "icon.ico"]
    end

    @impl true
    def get_file(_path) do
      %LivebookWeb.StaticProvidedPlug.File{content: "content", digest: "digest"}
    end
  end

  defmodule MyPlug do
    use Plug.Builder

    plug LivebookWeb.StaticProvidedPlug,
      at: "/",
      file_provider: MyProvider,
      gzip: true

    plug :passthrough

    defp passthrough(conn, _), do: Plug.Conn.send_resp(conn, 404, "Passthrough")
  end

  defp call(conn), do: MyPlug.call(conn, [])

  test "serves uncompressed file if there is no compressed version" do
    conn =
      conn(:get, "/icon.ico")
      |> put_req_header("accept-encoding", "gzip")
      |> call()

    assert conn.status == 200
    assert conn.resp_body == "content"
    assert get_resp_header(conn, "content-type") == ["image/vnd.microsoft.icon"]
    assert get_resp_header(conn, "etag") == [~s{"digest"}]
  end

  test "serves the compressed file if available" do
    conn =
      conn(:get, "/app.js")
      |> put_req_header("accept-encoding", "gzip")
      |> call()

    assert conn.status == 200
    assert get_resp_header(conn, "content-encoding") == ["gzip"]
    assert get_resp_header(conn, "content-type") == ["application/javascript"]
    assert get_resp_header(conn, "etag") == [~s{"digest"}]
  end

  test "ignores unavailable paths" do
    conn =
      conn(:get, "/invalid.js")
      |> call()

    assert conn.status == 404
  end
end
