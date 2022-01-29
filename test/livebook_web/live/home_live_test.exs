defmodule LivebookWeb.HomeLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.{Sessions, Session}

  test "disconnected and connected render", %{conn: conn} do
    {:ok, view, disconnected_html} = live(conn, "/")
    assert disconnected_html =~ "Running sessions"
    assert render(view) =~ "Running sessions"
  end

  test "redirects to session upon creation", %{conn: conn} do
    {:ok, view, _} = live(conn, "/")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element("button", "New notebook")
             |> render_click()

    assert to =~ "/sessions/"
  end

  describe "file selection" do
    test "updates the list of files as the input changes", %{conn: conn} do
      {:ok, view, _} = live(conn, "/")

      path = Path.expand("../../../lib", __DIR__) <> "/"

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: path})

      # Render the view separately to make sure it received the :set_file event
      render(view) =~ "livebook_web"
    end

    test "allows importing when a notebook file is selected", %{conn: conn} do
      {:ok, view, _} = live(conn, "/")

      path = test_notebook_path("basic")

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: Path.dirname(path) <> "/"})

      view
      |> element("button", "basic.livemd")
      |> render_click()

      assert assert {:error, {:live_redirect, %{to: to}}} =
                      view
                      |> element(~s{button[phx-click="fork"]}, "Fork")
                      |> render_click()

      assert to =~ "/sessions/"
    end

    @tag :tmp_dir
    test "disables import when a directory is selected", %{conn: conn, tmp_dir: tmp_dir} do
      {:ok, view, _} = live(conn, "/")

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: tmp_dir <> "/"})

      assert view
             |> element(~s{button[phx-click="fork"][disabled]}, "Fork")
             |> has_element?()
    end

    test "disables import when a nonexistent file is selected", %{conn: conn} do
      {:ok, view, _} = live(conn, "/")

      path = File.cwd!() |> Path.join("nonexistent.livemd")

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: path})

      assert view
             |> element(~s{button[phx-click="fork"][disabled]}, "Fork")
             |> has_element?()
    end

    @tag :tmp_dir
    test "disables open when a write-protected notebook is selected",
         %{conn: conn, tmp_dir: tmp_dir} do
      {:ok, view, _} = live(conn, "/")

      path = Path.join(tmp_dir, "write_protected.livemd")
      File.touch!(path)
      File.chmod!(path, 0o444)

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: tmp_dir <> "/"})

      view
      |> element("button", "write_protected.livemd")
      |> render_click()

      assert view
             |> element(~s{button[phx-click="open"][disabled]}, "Open")
             |> has_element?()

      assert view
             |> element(~s{[data-tooltip="This file is write-protected, please fork instead"]})
             |> has_element?()
    end
  end

  describe "sessions list" do
    test "lists running sessions", %{conn: conn} do
      {:ok, session1} = Sessions.create_session()
      {:ok, session2} = Sessions.create_session()

      {:ok, view, _} = live(conn, "/")

      assert render(view) =~ session1.id
      assert render(view) =~ session2.id
    end

    test "updates UI whenever a session is added or deleted", %{conn: conn} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "tracker_sessions")

      {:ok, view, _} = live(conn, "/")

      {:ok, %{id: id} = session} = Sessions.create_session()
      assert_receive {:session_created, %{id: ^id}}
      assert render(view) =~ id

      Session.close(session.pid)
      assert_receive {:session_closed, %{id: ^id}}
      refute render(view) =~ id
    end

    test "allows forking existing session", %{conn: conn} do
      {:ok, session} = Sessions.create_session()
      Session.set_notebook_name(session.pid, "My notebook")

      {:ok, view, _} = live(conn, "/")

      assert {:error, {:live_redirect, %{to: to}}} =
               view
               |> element(~s{[data-test-session-id="#{session.id}"] button}, "Fork")
               |> render_click()

      assert to =~ "/sessions/"

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "My notebook - fork"
    end

    test "allows closing session after confirmation", %{conn: conn} do
      {:ok, session} = Sessions.create_session()

      {:ok, view, _} = live(conn, "/")

      assert render(view) =~ session.id

      view
      |> element(~s{[data-test-session-id="#{session.id}"] a}, "Close")
      |> render_click()

      view
      |> element(~s{button[role=button]}, "Close session")
      |> render_click()

      refute render(view) =~ session.id
    end

    test "close all selected sessions using bulk action", %{conn: conn} do
      {:ok, session1} = Sessions.create_session()
      {:ok, session2} = Sessions.create_session()
      {:ok, session3} = Sessions.create_session()

      {:ok, view, _} = live(conn, "/")

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

      view
      |> element(~s{button[role="button"]}, "Close sessions")
      |> render_click()

      refute render(view) =~ session1.id
      refute render(view) =~ session2.id
      refute render(view) =~ session3.id
    end
  end

  test "link to introductory notebook correctly creates a new session", %{conn: conn} do
    {:ok, view, _} = live(conn, "/")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element(~s{[data-element="explore-section"] a}, "Welcome to Livebook")
             |> render_click()
             |> follow_redirect(conn)

    assert to =~ "/sessions/"

    {:ok, view, _} = live(conn, to)
    assert render(view) =~ "Welcome to Livebook"
  end

  describe "notebook import" do
    test "allows importing notebook directly from content", %{conn: conn} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "tracker_sessions")

      {:ok, view, _} = live(conn, "/home/import/content")

      notebook_content = """
      # My notebook
      """

      view
      |> element("form", "Import")
      |> render_submit(%{data: %{content: notebook_content}})

      assert_receive {:session_created, %{id: id}}

      {:ok, view, _} = live(conn, "/sessions/#{id}")
      assert render(view) =~ "My notebook"
    end

    test "should show info flash with information about the imported notebook", %{conn: conn} do
      {:ok, view, _} = live(conn, "/home/import/content")

      notebook_content = """
      # My notebook
      """

      view
      |> element("form", "Import")
      |> render_submit(%{data: %{content: notebook_content}})

      {_path, flash} = assert_redirect(view)

      assert flash["info"] =~
               "You have imported a notebook, no code has been executed so far. You should read and evaluate code as needed."
    end

    test "should show warning flash when the imported notebook have errors", %{conn: conn} do
      {:ok, view, _} = live(conn, "/home/import/content")

      # Notebook with 3 headers
      notebook_content = """
      # My notebook
      # My notebook
      # My notebook
      """

      view
      |> element("form", "Import")
      |> render_submit(%{data: %{content: notebook_content}})

      {_path, flash} = assert_redirect(view)

      assert flash["warning"] =~
               "We found problems while importing the file and tried to autofix them:\n- Downgrading all headings, because 3 instances of heading 1 were found"
    end
  end

  describe "public import endpoint" do
    test "imports notebook from the given url and redirects to the new session", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/notebook", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, "# My notebook")
      end)

      notebook_url = "http://localhost:#{bypass.port}/notebook"

      assert {:error, {:live_redirect, %{to: to}}} =
               live(conn, "/import?url=#{URI.encode_www_form(notebook_url)}")

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "My notebook"
    end

    @tag :tmp_dir
    test "imports notebook from local file URL", %{conn: conn, tmp_dir: tmp_dir} do
      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      File.write!(notebook_path, "# My notebook")
      notebook_url = "file://" <> notebook_path

      assert {:error, {:live_redirect, %{to: to}}} =
               live(conn, "/import?url=#{URI.encode_www_form(notebook_url)}")

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "My notebook"
    end

    test "redirects to the import form on error", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect(bypass, "GET", "/notebook", fn conn ->
        Plug.Conn.resp(conn, 500, "Error")
      end)

      notebook_url = "http://localhost:#{bypass.port}/notebook"

      assert {:error, {:live_redirect, %{to: to}}} =
               live(conn, "/import?url=#{URI.encode_www_form(notebook_url)}")

      assert to == "/home/import/url?url=#{URI.encode_www_form(notebook_url)}"

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ notebook_url
    end
  end

  # Helpers

  defp test_notebook_path(name) do
    path =
      ["../../support/notebooks", name <> ".livemd"]
      |> Path.join()
      |> Path.expand(__DIR__)

    unless File.exists?(path) do
      raise "Cannot find test notebook with the name: #{name}"
    end

    path
  end
end
