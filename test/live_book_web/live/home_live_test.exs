defmodule LiveBookWeb.HomeLiveTest do
  use LiveBookWeb.ConnCase

  import Phoenix.LiveViewTest

  test "disconnected and connected render", %{conn: conn} do
    {:ok, page_live, disconnected_html} = live(conn, "/")
    assert disconnected_html =~ "Welcome to LiveBook"
    assert render(page_live) =~ "Welcome to LiveBook"
  end
end
