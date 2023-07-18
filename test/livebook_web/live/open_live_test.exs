defmodule LivebookWeb.OpenLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.{Sessions, Session, FileSystem}

  describe "file selection" do
    test "updates the list of files as the input changes", %{conn: conn} do
      {:ok, view, _} = live(conn, ~p"/open/file")

      path = Path.expand("../../../lib", __DIR__) <> "/"

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: path})

      # Render the view separately to make sure it received the :set_file event
      render(view) =~ "livebook_web"
    end

    test "allows importing when a notebook file is selected", %{conn: conn} do
      {:ok, view, _} = live(conn, ~p"/open/file")

      path = test_notebook_path("basic")

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: Path.dirname(path) <> "/"})

      view
      |> element("button", "basic.livemd")
      |> render_click()

      view
      |> element(~s{button[phx-click="fork"]}, "Fork")
      |> render_click()

      {to, _flash} = assert_redirect(view)

      assert to =~ "/sessions/"

      close_session_by_path(to)
    end

    @tag :tmp_dir
    test "disables import when a directory is selected", %{conn: conn, tmp_dir: tmp_dir} do
      {:ok, view, _} = live(conn, ~p"/open/file")

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: tmp_dir <> "/"})

      assert view
             |> element(~s{button[phx-click="fork"][disabled]}, "Fork")
             |> has_element?()
    end

    test "disables import when a nonexistent file is selected", %{conn: conn} do
      {:ok, view, _} = live(conn, ~p"/open/file")

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
      {:ok, view, _} = live(conn, ~p"/open/file")

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

  describe "notebook import" do
    test "allows importing notebook directly from source", %{conn: conn} do
      {:ok, view, _} = live(conn, ~p"/open/source")

      notebook_source = """
      # My notebook
      """

      view
      |> element("form", "Import")
      |> render_submit(%{data: %{source: notebook_source}})

      {path, _flash} = assert_redirect(view, 5000)

      {:ok, view, _} = live(conn, path)
      assert render(view) =~ "My notebook"

      close_session_by_path(path)
    end

    test "should show info flash with information about the imported notebook", %{conn: conn} do
      {:ok, view, _} = live(conn, ~p"/open/source")

      notebook_source = """
      # My notebook
      """

      view
      |> element("form", "Import")
      |> render_submit(%{data: %{source: notebook_source}})

      {path, flash} = assert_redirect(view, 5000)

      assert flash["info"] =~
               "You have imported a notebook, no code has been executed so far. You should read and evaluate code as needed."

      close_session_by_path(path)
    end

    test "should show warning flash when the imported notebook have errors", %{conn: conn} do
      {:ok, view, _} = live(conn, ~p"/open/source")

      # Notebook with 3 headers
      notebook_source = """
      # My notebook
      # My notebook
      # My notebook
      """

      view
      |> element("form", "Import")
      |> render_submit(%{data: %{source: notebook_source}})

      {path, flash} = assert_redirect(view, 5000)

      assert flash["warning"] =~
               "We found problems while importing the file:\n- downgrading all headings, because 3 instances of heading 1 were found"

      close_session_by_path(path)
    end

    test "allows importing notebook from url and downloads its files", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, """
        <!-- livebook:{"file_entries":[{"name":"image.jpg","type":"attachment"}]} -->

        # My notebook
        """)
      end)

      Bypass.expect_once(bypass, "GET", "/files/image.jpg", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, "content")
      end)

      notebook_url = "http://localhost:#{bypass.port}/notebook.livemd"

      {:ok, view, _} = live(conn, ~p"/open/url")

      view
      |> element("form", "Import")
      |> render_submit(%{data: %{url: notebook_url}})

      {path, _flash} = assert_redirect(view, 5000)

      "/sessions/" <> session_id = path
      {:ok, session} = Sessions.fetch_session(session_id)

      assert FileSystem.File.resolve(session.files_dir, "image.jpg") |> FileSystem.File.read() ==
               {:ok, "content"}

      close_session_by_path(path)
    end

    test "allows importing notebook from upload", %{conn: conn} do
      {:ok, view, _} = live(conn, ~p"/open/upload")

      view
      |> file_input("#upload-file-form", :file, [
        %{
          last_modified: 1_594_171_879_000,
          name: "notebook.livemd",
          content: """
          # My notebook
          """,
          size: 14,
          type: "text/plain"
        }
      ])
      |> render_upload("notebook.livemd")

      view
      |> element("#upload-file-form")
      |> render_submit()

      {path, _flash} = assert_redirect(view, 5000)

      {:ok, view, _} = live(conn, path)
      assert render(view) =~ "My notebook"

      close_session_by_path(path)
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

      assert {:error, {:live_redirect, %{to: to}}} = live(conn, ~p"/import?url=#{notebook_url}")

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "My notebook"

      close_session_by_path(to)
    end

    @tag :tmp_dir
    test "imports notebook from local file URL", %{conn: conn, tmp_dir: tmp_dir} do
      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      File.write!(notebook_path, "# My notebook")
      notebook_url = "file://" <> notebook_path

      assert {:error, {:live_redirect, %{to: to}}} = live(conn, ~p"/import?url=#{notebook_url}")

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "My notebook"

      close_session_by_path(to)
    end

    test "redirects to the import form on error", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect(bypass, "GET", "/notebook", fn conn ->
        Plug.Conn.resp(conn, 500, "Error")
      end)

      notebook_url = "http://localhost:#{bypass.port}/notebook"

      assert {:error, {:live_redirect, %{to: to}}} = live(conn, ~p"/import?url=#{notebook_url}")

      assert to == ~p"/open/url?url=#{notebook_url}"

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ notebook_url
    end
  end

  describe "public open endpoint" do
    test "checkouts the directory when a directory is passed", %{conn: conn} do
      directory_path = Path.join(File.cwd!(), "test")

      assert {:error, {:live_redirect, %{to: to}}} = live(conn, ~p"/open?path=#{directory_path}")

      assert to == ~p"/open/file?path=#{directory_path}"

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ directory_path
    end

    @tag :tmp_dir
    test "opens a file when livebook file is passed", %{conn: conn, tmp_dir: tmp_dir} do
      notebook_path = Path.join(tmp_dir, "notebook.livemd")

      :ok = File.write(notebook_path, "# My notebook")

      assert {:error, {:live_redirect, %{flash: %{}, to: to}}} =
               live(conn, ~p"/open?path=#{notebook_path}")

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "My notebook"

      close_session_by_path(to)
    end
  end

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

  defp close_session_by_path("/sessions/" <> session_id) do
    {:ok, session} = Sessions.fetch_session(session_id)
    Session.close(session.pid)
  end
end
