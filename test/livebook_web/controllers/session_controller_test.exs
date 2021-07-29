defmodule LivebookWeb.SessionControllerTest do
  use LivebookWeb.ConnCase, async: true

  alias Livebook.{SessionSupervisor, Session, Notebook}

  describe "show_image" do
    test "returns not found when the given session does not exist", %{conn: conn} do
      conn = get(conn, Routes.session_path(conn, :show_image, "nonexistent", "image.jpg"))

      assert conn.status == 404
      assert conn.resp_body == "Not found"
    end

    test "returns not found when the given image does not exist", %{conn: conn} do
      {:ok, session_id} = SessionSupervisor.create_session()

      conn = get(conn, Routes.session_path(conn, :show_image, session_id, "nonexistent.jpg"))

      assert conn.status == 404
      assert conn.resp_body == "Not found"

      SessionSupervisor.close_session(session_id)
    end

    test "returns the image when it does exist", %{conn: conn} do
      {:ok, session_id} = SessionSupervisor.create_session()
      %{images_dir: images_dir} = Session.get_summary(session_id)
      File.mkdir_p!(images_dir)
      images_dir |> Path.join("test.jpg") |> File.touch!()

      conn = get(conn, Routes.session_path(conn, :show_image, session_id, "test.jpg"))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["image/jpeg"]

      SessionSupervisor.close_session(session_id)
    end
  end

  describe "download_source" do
    test "returns not found when the given session does not exist", %{conn: conn} do
      conn = get(conn, Routes.session_path(conn, :download_source, "nonexistent", "livemd"))

      assert conn.status == 404
      assert conn.resp_body == "Not found"
    end

    test "returns bad request when given an invalid format", %{conn: conn} do
      {:ok, session_id} = SessionSupervisor.create_session()

      conn = get(conn, Routes.session_path(conn, :download_source, session_id, "invalid"))

      assert conn.status == 400
      assert conn.resp_body == "Invalid format, supported formats: livemd, exs"
    end

    test "handles live markdown notebook source", %{conn: conn} do
      {:ok, session_id} = SessionSupervisor.create_session()

      conn = get(conn, Routes.session_path(conn, :download_source, session_id, "livemd"))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/plain"]

      assert conn.resp_body == """
             # Untitled notebook
             """

      SessionSupervisor.close_session(session_id)
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

      {:ok, session_id} = SessionSupervisor.create_session(notebook: notebook)

      query = [include_outputs: "true"]
      conn = get(conn, Routes.session_path(conn, :download_source, session_id, "livemd", query))

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

      SessionSupervisor.close_session(session_id)
    end

    test "handles elixir notebook source", %{conn: conn} do
      {:ok, session_id} = SessionSupervisor.create_session()

      conn = get(conn, Routes.session_path(conn, :download_source, session_id, "exs"))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/plain"]

      assert conn.resp_body == """
             # Title: Untitled notebook
             """

      SessionSupervisor.close_session(session_id)
    end
  end
end
