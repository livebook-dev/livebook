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
                    Notebook.Cell.new(:code)
                    | source: """
                      IO.puts("hey")\
                      """,
                      outputs: [{0, {:stdout, "hey"}}]
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

             <!-- livebook:{"output":true} -->

             ```
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
             # Run as: iex --dot-iex path/to/notebook.exs

             # Title: Untitled notebook

             # ── Section ──
             """

      Session.close(session.pid)
    end
  end

  describe "show_asset" do
    test "fetches assets and redirects to the session-less path", %{conn: conn} do
      %{notebook: notebook, hash: hash} = notebook_with_js_output()

      conn = start_session_and_request_asset(conn, notebook, hash)

      assert redirected_to(conn, 301) ==
               Routes.session_path(conn, :show_cached_asset, hash, ["main.js"])
    end

    test "skips the session if assets are in cache", %{conn: conn} do
      %{notebook: notebook, hash: hash} = notebook_with_js_output()
      # Fetch the assets for the first time
      conn = start_session_and_request_asset(conn, notebook, hash)

      # Use nonexistent session, so any communication would fail
      random_session_id = Livebook.Utils.random_node_aware_id()

      conn =
        get(conn, Routes.session_path(conn, :show_asset, random_session_id, hash, ["main.js"]))

      assert redirected_to(conn, 301) ==
               Routes.session_path(conn, :show_cached_asset, hash, ["main.js"])
    end
  end

  describe "show_cached_asset" do
    test "returns not found when no matching assets are in the cache", %{conn: conn} do
      %{notebook: _notebook, hash: hash} = notebook_with_js_output()

      conn = get(conn, Routes.session_path(conn, :show_cached_asset, hash, ["main.js"]))

      assert conn.status == 404
      assert conn.resp_body == "Not found"
    end

    test "returns the requestes asset if available in cache", %{conn: conn} do
      %{notebook: notebook, hash: hash} = notebook_with_js_output()
      # Fetch the assets for the first time
      conn = start_session_and_request_asset(conn, notebook, hash)

      conn = get(conn, Routes.session_path(conn, :show_cached_asset, hash, ["main.js"]))

      assert conn.status == 200
      assert "export function init(" <> _ = conn.resp_body
    end
  end

  defp start_session_and_request_asset(conn, notebook, hash) do
    {:ok, session} = Sessions.create_session(notebook: notebook)
    # We need runtime in place to actually copy the archive
    {:ok, runtime} = Livebook.Runtime.Embedded.new() |> Livebook.Runtime.connect()
    Session.set_runtime(session.pid, runtime)

    conn = get(conn, Routes.session_path(conn, :show_asset, session.id, hash, ["main.js"]))

    Session.close(session.pid)

    conn
  end

  defp notebook_with_js_output() do
    archive_path = Path.expand("../../support/assets.tar.gz", __DIR__)
    hash = "test-" <> Livebook.Utils.random_id()
    assets_info = %{archive_path: archive_path, hash: hash, js_path: "main.js"}
    output = {:js, %{js_view: %{assets: assets_info}}}

    notebook = %{
      Notebook.new()
      | sections: [
          %{
            Notebook.Section.new()
            | cells: [%{Notebook.Cell.new(:code) | outputs: [{0, output}]}]
          }
        ]
    }

    %{notebook: notebook, hash: hash}
  end
end
