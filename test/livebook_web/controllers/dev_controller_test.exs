defmodule LivebookWeb.DevControllerTest do
  use LivebookWeb.ConnCase, async: true

  alias Livebook.{Sessions, Session, FileSystem}

  setup do
    Livebook.Settings.set_dev_endpoints_enabled(true)
    on_exit(fn -> Livebook.Settings.set_dev_endpoints_enabled(false) end)
  end

  test "rejects requests with origin header", %{conn: conn} do
    conn =
      conn
      |> put_req_header("origin", "http://localhost:4000")
      |> post(~p"/dev/sync", %{file: "/some/path"})

    assert json_response(conn, 403) == %{
             "status" => "error",
             "message" => "This endpoint is not available in the browser"
           }
  end

  test "rejects requests when dev endpoints are disabled", %{conn: conn} do
    Livebook.Settings.set_dev_endpoints_enabled(false)

    conn = post(conn, ~p"/dev/sync", %{file: "/some/path"})

    assert json_response(conn, 403) == %{
             "status" => "error",
             "message" => "Dev endpoints are disabled, you can enable them in the settings"
           }
  end

  describe "sync" do
    @tag :tmp_dir
    test "syncs the session when it exists for the given file", %{conn: conn, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      {:ok, session} = Sessions.create_session(file: file)

      on_exit(fn -> Session.close(session.pid) end)

      :ok =
        FileSystem.File.write(file, """
        # My notebook

        ## Section

        Hello world!
        """)

      conn = post(conn, ~p"/dev/sync", %{file: file.path})

      assert json_response(conn, 200) == %{"status" => "ok"}

      assert %{notebook: %{name: "My notebook"}} = Session.get_data(session.pid)
    end

    @tag :tmp_dir
    test "returns error when no session exists for the given file",
         %{conn: conn, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      conn = post(conn, ~p"/dev/sync", %{file: file.path})

      assert json_response(conn, 404) == %{
               "status" => "error",
               "message" => "No session found for the given file"
             }
    end
  end

  describe "open" do
    @tag :tmp_dir
    test "returns the existing session path when a session with the file already exists",
         %{conn: conn, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      {:ok, session} = Sessions.create_session(file: file)

      on_exit(fn -> Session.close(session.pid) end)

      conn = post(conn, ~p"/dev/open", %{file: file.path})

      assert json_response(conn, 200) == %{"path" => ~p"/sessions/#{session.id}"}
    end

    @tag :tmp_dir
    test "creates a new session and returns its path when no session exists",
         %{conn: conn, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      :ok =
        FileSystem.File.write(file, """
        # My notebook

        ## Section

        ```elixir
        :ok
        ```
        """)

      conn = post(conn, ~p"/dev/open", %{file: file.path})

      assert %{"path" => "/sessions/" <> session_id} = json_response(conn, 200)

      {:ok, session} = Sessions.fetch_session(session_id)
      assert session.file == file
      assert %{notebook: %{name: "My notebook"}} = Session.get_data(session.pid)

      Session.close(session.pid)
    end

    @tag :tmp_dir
    test "returns error when the file does not exist", %{conn: conn, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "nonexistent.livemd")

      conn = post(conn, ~p"/dev/open", %{file: file.path})

      assert json_response(conn, 422) == %{
               "status" => "error",
               "message" => "Failed to read file: no such file or directory"
             }
    end
  end

  describe "restamp" do
    test "returns new_source content as is if old_source has no stamp", %{conn: conn} do
      old_source = """
      # Notebook before

      ## Section

      ```elixir
      :original
      ```
      """

      new_source = """
      # Notebook after

      ## Section

      ```elixir
      :changed
      ```
      """

      conn =
        post(conn, ~p"/dev/restamp", %{
          old_source: old_source,
          new_source: new_source
        })

      assert %{"source" => source} = json_response(conn, 200)
      assert source == new_source
    end

    test "returns error when old_source has an invalid stamp", %{conn: conn} do
      old_source = """
      # Notebook before

      ## Section

      <!-- livebook:{"offset":61,"stamp":"invalid_stamp"} -->
      """

      conn =
        post(conn, ~p"/dev/restamp", %{
          old_source: old_source,
          new_source: "# Notebook after\n"
        })

      assert json_response(conn, 422) == %{
               "status" => "error",
               "message" => "The old_source stamp is invalid"
             }
    end

    test "returns new_source unchanged when it already has a valid stamp", %{conn: conn} do
      notebook = %{
        Livebook.Notebook.new()
        | hub_id: "personal-hub",
          hub_secret_names: ["MY_SECRET"]
      }

      {old_source, _} = Livebook.LiveMarkdown.notebook_to_livemd(notebook)
      {new_source, _} = Livebook.LiveMarkdown.notebook_to_livemd(notebook)

      conn =
        post(conn, ~p"/dev/restamp", %{
          old_source: old_source,
          new_source: new_source
        })

      assert json_response(conn, 200) == %{"source" => new_source}
    end

    test "returns stamped new_source preserving metadata from old_source", %{conn: conn} do
      notebook_before = %{
        Livebook.Notebook.new()
        | hub_id: "personal-hub",
          hub_secret_names: ["MY_SECRET"]
      }

      {old_source, _} = Livebook.LiveMarkdown.notebook_to_livemd(notebook_before)

      new_source = """
      # Updated notebook

      ## Section

      Hello world!
      """

      conn =
        post(conn, ~p"/dev/restamp", %{
          old_source: old_source,
          new_source: new_source
        })

      assert %{"source" => source} = json_response(conn, 200)

      assert source =~ "# Updated notebook"

      # Verify the returned source has valid stamp with the same metadata.
      {notebook, _} = Livebook.LiveMarkdown.notebook_from_livemd(source)
      assert %{hub_id: "personal-hub", hub_secret_names: ["MY_SECRET"]} = notebook
    end
  end
end
