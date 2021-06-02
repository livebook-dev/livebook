defmodule LivebookWeb.ExploreLiveTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  test "link to introductory notebook correctly creates a new session", %{conn: conn} do
    {:ok, view, _} = live(conn, "/explore")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element(~s{a}, "Let's go")
             |> render_click()

    assert to =~ "/sessions/"

    {:ok, view, _} = live(conn, to)
    assert render(view) =~ "Welcome to Livebook"
  end
end
