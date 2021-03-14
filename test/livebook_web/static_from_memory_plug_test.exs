defmodule LivebookWeb.StaticFromMemoryPlugTest do
  use ExUnit.Case, async: true
  use Plug.Test

  # Make sure to run `mix phx.digest test/support/static -o test/support/static`
  # after changing files in `test/support/static`.

  defmodule MyPlug do
    use Plug.Builder

    plug LivebookWeb.StaticFromMemoryPlug,
      at: "/",
      files: LivebookWeb.StaticFromMemoryPlug.preload_files!(
        only: ~w(app.js icon.ico),
        from: Path.expand("../support/static", __DIR__)
      ),
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
    assert conn.resp_body == ""
    assert get_resp_header(conn, "content-type") == ["image/vnd.microsoft.icon"]
  end

  test "serves the compressed file if available" do
    conn =
      conn(:get, "/app.js")
      |> put_req_header("accept-encoding", "gzip")
      |> call()

    assert conn.status == 200
    assert :zlib.gunzip(conn.resp_body) == ~s{console.log("Hello");\n}
    assert get_resp_header(conn, "content-type") == ["application/javascript"]
  end

  test "does not include uncompressed file when a compressed version is avaiable" do
    conn =
      conn(:get, "/app.js")
      |> call()

    assert conn.status == 404
  end

  test "does not include the digested file" do
    conn =
      conn(:get, "/app-3dbaca9bcb436d56b5bd95bf083073a0.js")
      |> put_req_header("accept-encoding", "gzip")
      |> call()

    assert conn.status == 404
  end

  test "excludes files not matched by :only" do
    conn =
      conn(:get, "/excluded.js")
      |> put_req_header("accept-encoding", "gzip")
      |> call()

    assert conn.status == 404
  end
end
