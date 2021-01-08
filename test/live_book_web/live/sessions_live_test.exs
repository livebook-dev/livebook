defmodule LiveBookWeb.SessionsLiveTest do
  use LiveBookWeb.ConnCase

  import Phoenix.LiveViewTest

  test "disconnected and connected render", %{conn: conn} do
    {:ok, view, disconnected_html} = live(conn, "/sessions")
    assert disconnected_html =~ "Sessions"
    assert render(view) =~ "Sessions"
  end

  test "lists running sessions", %{conn: conn} do
    {:ok, id1} = LiveBook.SessionSupervisor.create_session()
    {:ok, id2} = LiveBook.SessionSupervisor.create_session()

    {:ok, view, _} = live(conn, "/sessions")

    assert render(view) =~ id1
    assert render(view) =~ id2
  end

  test "redirects to session upon creation", %{conn: conn} do
    {:ok, view, _} = live(conn, "/sessions")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element("button", "New session")
             |> render_click()

    assert to =~ "/sessions/"
  end

  test "updates UI whenever a session is added or deleted", %{conn: conn} do
    {:ok, view, _} = live(conn, "/sessions")

    {:ok, id} = LiveBook.SessionSupervisor.create_session()
    assert render(view) =~ id

    LiveBook.SessionSupervisor.delete_session(id)
    refute render(view) =~ id
  end
end
