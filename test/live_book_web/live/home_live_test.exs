defmodule LiveBookWeb.HomeLiveTest do
  use LiveBookWeb.ConnCase

  import Phoenix.LiveViewTest

  test "disconnected and connected render", %{conn: conn} do
    {:ok, view, disconnected_html} = live(conn, "/")
    assert disconnected_html =~ "Welcome to LiveBook"
    assert render(view) =~ "Welcome to LiveBook"
  end
end
