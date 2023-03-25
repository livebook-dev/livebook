defmodule LivebookWeb.ErrorsTest do
  use LivebookWeb.ConnCase, async: true

  test "renders 404", %{conn: conn} do
    response =
      assert_error_sent :not_found, fn ->
        get(conn, "/this/does/not/exist")
      end

    assert {404, _, body} = response
    assert body =~ "No Numbats here!"
  end
end
