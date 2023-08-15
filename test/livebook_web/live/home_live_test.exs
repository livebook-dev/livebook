defmodule LivebookWeb.HomeLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.SessionHelpers
  import Livebook.TestHelpers

  alias Livebook.{Sessions, Session}

  test "disconnected and connected render", %{conn: conn} do
    {:ok, view, disconnected_html} = live(conn, ~p"/")
    assert disconnected_html =~ "Running sessions"
    assert render(view) =~ "Running sessions"
  end

  test "redirects to session upon creation", %{conn: conn} do
    {:ok, view, _} = live(conn, ~p"/")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element(~s/[role="navigation"] a/, "New notebook")
             |> render_click()

    assert to =~ "/sessions/"

    close_session_by_path(to)
  end

  test "public new endpoint creates an empty session", %{conn: conn} do
    assert {:error, {:live_redirect, %{to: to}}} = result = live(conn, ~p"/new")
    {:ok, view, _} = follow_redirect(result, conn)

    assert render(view) =~ "Untitled notebook"

    close_session_by_path(to)
  end

  describe "sessions list" do
    test "lists running sessions", %{conn: conn} do
      {:ok, session1} = Sessions.create_session()
      {:ok, session2} = Sessions.create_session()

      {:ok, view, _} = live(conn, ~p"/")

      assert render(view) =~ session1.id
      assert render(view) =~ session2.id

      Session.close([session1.pid, session2.pid])
    end

    test "updates UI whenever a session is added or deleted", %{conn: conn} do
      Sessions.subscribe()

      {:ok, view, _} = live(conn, ~p"/")

      {:ok, %{id: id} = session} = Sessions.create_session()
      assert_receive {:session_created, %{id: ^id}}
      assert render(view) =~ id

      Session.close(session.pid)
      assert_receive {:session_closed, %{id: ^id}}
      refute render(view) =~ id
    end

    test "allows download the source of an existing session", %{conn: conn} do
      {:ok, session} = Sessions.create_session()
      Session.set_notebook_name(session.pid, "My notebook")

      {:ok, view, _} = live(conn, ~p"/")

      {:error, {:redirect, %{to: to}}} =
        view
        |> element(~s{[data-test-session-id="#{session.id}"] a}, "Download source")
        |> render_click

      assert to == ~p"/sessions/#{session.id}/download/export/livemd?include_outputs=false"

      Session.close(session.pid)
    end

    test "allows forking existing session", %{conn: conn} do
      {:ok, session} = Sessions.create_session()
      Session.set_notebook_name(session.pid, "My notebook")

      {:ok, view, _} = live(conn, ~p"/")

      assert {:error, {:live_redirect, %{to: to}}} =
               view
               |> element(~s{[data-test-session-id="#{session.id}"] button}, "Fork")
               |> render_click()

      assert to =~ "/sessions/"

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "My notebook - fork"

      close_session_by_path(to)
      Session.close(session.pid)
    end

    test "allows closing session after confirmation", %{conn: conn} do
      {:ok, session} = Sessions.create_session()

      {:ok, view, _} = live(conn, ~p"/")

      assert render(view) =~ session.id

      view
      |> element(~s{[data-test-session-id="#{session.id}"] button}, "Close")
      |> render_click()

      render_confirm(view)

      refute render(view) =~ session.id
    end

    test "close all selected sessions using bulk action", %{conn: conn} do
      {:ok, session1} = Sessions.create_session()
      {:ok, session2} = Sessions.create_session()
      {:ok, session3} = Sessions.create_session()

      {:ok, view, _} = live(conn, ~p"/")

      assert render(view) =~ session1.id
      assert render(view) =~ session2.id
      assert render(view) =~ session3.id

      view
      |> form("#bulk-action-form", %{
        "action" => "close_all",
        "session_ids" => [session1.id, session2.id, session3.id]
      })
      |> render_submit()

      assert render(view) =~ "Are you sure you want to close 3 sessions?"

      render_confirm(view)

      refute render(view) =~ session1.id
      refute render(view) =~ session2.id
      refute render(view) =~ session3.id
    end
  end

  describe "starred notebooks" do
    @tag :tmp_dir
    test "starring and unstarring a notebook", %{conn: conn, tmp_dir: tmp_dir} do
      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      file = Livebook.FileSystem.File.local(notebook_path)

      {:ok, session} = Sessions.create_session()
      Session.set_file(session.pid, file)
      Session.set_notebook_name(session.pid, "Special notebook")

      wait_for_session_update(session.pid)

      {:ok, view, _} = live(conn, ~p"/")

      Livebook.NotebookManager.subscribe_starred_notebooks()

      Livebook.NotebookManager.add_starred_notebook(file, "Special notebook")
      assert_receive {:starred_notebooks_updated, _}

      assert view
             |> element(~s/#starred-notebooks/, "Special notebook")
             |> has_element?()

      Livebook.NotebookManager.remove_starred_notebook(file)
      assert_receive {:starred_notebooks_updated, _}

      refute view
             |> element(~s/#starred-notebooks/, "Special notebook")
             |> has_element?()
    end

    @tag :tmp_dir
    test "opening a starred notebook", %{conn: conn, tmp_dir: tmp_dir} do
      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      File.write!(notebook_path, "# Starred notebook")
      file = Livebook.FileSystem.File.local(notebook_path)
      Livebook.NotebookManager.add_starred_notebook(file, "Starred notebook")

      {:ok, view, _} = live(conn, ~p"/")

      card_html =
        view
        |> element(~s/#starred-notebooks [data-test-idx]/, "Starred notebook")
        |> render()

      assert [attr] = Regex.run(~r/data-test-idx="\d+"/, card_html)

      view
      |> element(~s/#starred-notebooks [#{attr}] button/, "Open")
      |> render_click()

      {to, _flash} = assert_redirect(view)

      assert to =~ "/sessions/"

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "Starred notebook"

      close_session_by_path(to)
    end
  end

  describe "hubs" do
    test "renders sidebar section", %{conn: conn} do
      {:ok, _view, html} = live(conn, ~p"/")
      assert html =~ "HUBS"
      assert html =~ "Add Organization"
    end

    test "renders sidebar persisted hubs", %{conn: conn} do
      team = insert_hub(:team, id: "team-foo-bar-id")

      {:ok, _view, html} = live(conn, ~p"/")
      assert html =~ "HUBS"
      assert html =~ team.hub_name

      Livebook.Hubs.delete_hub("team-foo-bar-id")
    end
  end

  test "handles user profile update", %{conn: conn} do
    {:ok, view, _} = live(conn, ~p"/")
    data = %{user: %{name: "Jake Peralta", hex_color: "#123456"}}

    view
    |> element("#user_form")
    |> render_change(data)

    view
    |> element("#user_form")
    |> render_submit(data)

    assert render(view) =~ "Jake Peralta"
    assert render(view) =~ "#123456"
  end

  # Helpers

  defp close_session_by_path("/sessions/" <> session_id) do
    {:ok, session} = Sessions.fetch_session(session_id)
    Session.close(session.pid)
  end
end
