defmodule LivebookWeb.ErrorsTest do
  use LivebookWeb.ConnCase, async: true

  test "renders 404", %{conn: conn} do
    conn = get(conn, "/this/does/not/exist")

    assert conn.status == 404
    assert conn.resp_body =~ "No Numbats here!"
  end
end
