defmodule LivebookWeb.LearnLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  test "link to introductory notebook correctly creates a new session", %{conn: conn} do
    {:ok, view, _} = live(conn, "/learn")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element(~s{#welcome-to-livebook a}, "Open notebook")
             |> render_click()

    assert to =~ "/sessions/"

    {:ok, view, _} = live(conn, to)
    assert render(view) =~ "Welcome to Livebook"

    close_session_by_path(to)
  end

  test "link to a new notebook creates an empty session", %{conn: conn} do
    assert {:error, {:live_redirect, %{to: to}}} = result = live(conn, "/learn/notebooks/new")
    {:ok, view, _} = follow_redirect(result, conn)

    assert render(view) =~ "Untitled notebook"

    close_session_by_path(to)
  end

  defp close_session_by_path("/sessions/" <> session_id) do
    {:ok, session} = Livebook.Sessions.fetch_session(session_id)
    Livebook.Session.close(session.pid)
  end
end
