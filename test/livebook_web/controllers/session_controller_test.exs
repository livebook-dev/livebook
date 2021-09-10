defmodule LivebookWeb.SessionControllerTest do
  use LivebookWeb.ConnCase, async: true

  alias Livebook.{Sessions, Session, Notebook, FileSystem}

  describe "show_image" do
    test "returns not found when the given session does not exist", %{conn: conn} do
      id = Livebook.Utils.random_node_aware_id()
      conn = get(conn, Routes.session_path(conn, :show_image, id, "image.jpg"))

      assert conn.status == 404
      assert conn.resp_body == "Not found"
    end

    test "returns not found when the given image does not exist", %{conn: conn} do
      {:ok, session} = Sessions.create_session()

      conn = get(conn, Routes.session_path(conn, :show_image, session.id, "nonexistent.jpg"))

      assert conn.status == 404
      assert conn.resp_body == "No such file or directory"

      Session.close(session.pid)
    end

    test "returns the image when it does exist", %{conn: conn} do
      {:ok, session} = Sessions.create_session()
      %{images_dir: images_dir} = session
      :ok = FileSystem.File.resolve(images_dir, "test.jpg") |> FileSystem.File.write("")

      conn = get(conn, Routes.session_path(conn, :show_image, session.id, "test.jpg"))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["image/jpeg"]

      Session.close(session.pid)
    end
  end

  describe "download_source" do
    test "returns not found when the given session does not exist", %{conn: conn} do
      id = Livebook.Utils.random_node_aware_id()
      conn = get(conn, Routes.session_path(conn, :download_source, id, "livemd"))

      assert conn.status == 404
      assert conn.resp_body == "Not found"
    end

    test "returns bad request when given an invalid format", %{conn: conn} do
      {:ok, session} = Sessions.create_session()

      conn = get(conn, Routes.session_path(conn, :download_source, session.id, "invalid"))

      assert conn.status == 400
      assert conn.resp_body == "Invalid format, supported formats: livemd, exs"
    end

    test "handles live markdown notebook source", %{conn: conn} do
      {:ok, session} = Sessions.create_session()

      conn = get(conn, Routes.session_path(conn, :download_source, session.id, "livemd"))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/plain"]

      assert conn.resp_body == """
             # Untitled notebook

             ## Section

             ```elixir

             ```
             """

      Session.close(session.pid)
    end

    test "includes output in markdown when include_outputs parameter is set", %{conn: conn} do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: """
                      IO.puts("hey")\
                      """,
                      outputs: ["hey"]
                  }
                ]
            }
          ]
      }

      {:ok, session} = Sessions.create_session(notebook: notebook)

      query = [include_outputs: "true"]
      conn = get(conn, Routes.session_path(conn, :download_source, session.id, "livemd", query))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/plain"]

      assert conn.resp_body == """
             # My Notebook

             ## Section 1

             ```elixir
             IO.puts("hey")
             ```

             ```output
             hey
             ```
             """

      Session.close(session.pid)
    end

    test "handles elixir notebook source", %{conn: conn} do
      {:ok, session} = Sessions.create_session()

      conn = get(conn, Routes.session_path(conn, :download_source, session.id, "exs"))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/plain"]

      assert conn.resp_body == """
             # Title: Untitled notebook

             # ── Section ──
             """

      Session.close(session.pid)
    end
  end
end
