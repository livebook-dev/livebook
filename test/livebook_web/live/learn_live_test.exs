defmodule LivebookWeb.LearnLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  test "link to introductory notebook correctly creates a new session", %{conn: conn} do
    {:ok, view, _} = live(conn, ~p"/learn")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element(~s{#welcome-to-livebook a}, "Open notebook")
             |> render_click()

    assert to =~ "/sessions/"

    # Note that this LV page is huge and the simulated rendering in
    # LV tests is not heavily optimized. This LV receives events from
    # concurrent tests (such as hub creation) and rendering is a big
    # bottleneck, to the point where calling render(view) times out.
    # That's why we only assert on the dead render HTML.
    {:ok, _view, html} = live(conn, to)
    assert html =~ "Welcome to Livebook"

    close_session_by_path(to)
  end

  defp close_session_by_path("/sessions/" <> session_id) do
    {:ok, session} = Livebook.Sessions.fetch_session(session_id)
    Livebook.Session.close(session.pid)
  end
end
