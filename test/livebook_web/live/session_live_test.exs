defmodule LivebookWeb.SessionLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Livebook.SessionHelpers
  import Phoenix.LiveViewTest

  alias Livebook.{Sessions, Session, Settings, Runtime, Users, FileSystem}
  alias Livebook.Notebook.Cell

  setup do
    {:ok, session} = Sessions.create_session(notebook: Livebook.Notebook.new())

    on_exit(fn ->
      Session.close(session.pid)
    end)

    %{session: session}
  end

  test "disconnected and connected render", %{conn: conn, session: session} do
    {:ok, view, disconnected_html} = live(conn, ~p"/sessions/#{session.id}")
    assert disconnected_html =~ "Untitled notebook"
    assert render(view) =~ "Untitled notebook"
  end

  describe "asynchronous updates" do
    test "renders an updated notebook name", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.set_notebook_name(session.pid, "My notebook")
      wait_for_session_update(session.pid)

      assert render(view) =~ "My notebook"
    end

    test "renders an updated notebook name in title", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert page_title(view) =~ "Untitled notebook"

      Session.set_notebook_name(session.pid, "My notebook")
      wait_for_session_update(session.pid)

      # Wait for LV to update
      _ = render(view)

      assert page_title(view) =~ "My notebook"
    end

    test "renders a newly inserted section", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      section_id = insert_section(session.pid)

      assert render(view) =~ section_id
    end

    test "renders an updated section name", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.set_section_name(session.pid, section_id, "My section")
      wait_for_session_update(session.pid)

      assert render(view) =~ "My section"
    end

    test "renders a newly inserted cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      cell_id = insert_text_cell(session.pid, section_id, :markdown)

      assert render(view) =~ "cell-" <> cell_id
    end

    test "un-renders a deleted cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :markdown)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.delete_cell(session.pid, cell_id)
      wait_for_session_update(session.pid)

      refute render(view) =~ "cell-" <> cell_id
    end
  end

  describe "UI events update the shared state" do
    test "adding a new section", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("button", "New section")
      |> render_click()

      assert %{notebook: %{sections: [_section]}} = Session.get_data(session.pid)
    end

    test "adding a new cell", %{conn: conn, session: session} do
      Session.insert_section(session.pid, 0)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("button", "Markdown")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%Cell.Markdown{}]}]}} =
               Session.get_data(session.pid)
    end

    test "queueing cell evaluation", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code, "Process.sleep(50)")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert %{cell_infos: %{^cell_id => %{eval: %{status: :evaluating}}}} =
               Session.get_data(session.pid)
    end

    test "reevaluting the setup cell", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => "setup"})

      assert_receive {:operation, {:set_runtime, _pid, %{} = _runtime}}
    end

    test "reevaluting the setup cell with dependencies cache disabled",
         %{conn: conn, session: session} do
      Session.subscribe(session.id)

      # Start the standalone runtime, to encapsulate env var changes
      {:ok, runtime} = Runtime.ElixirStandalone.new() |> Runtime.connect()
      Session.set_runtime(session.pid, runtime)

      evaluate_setup(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{
        "cell_id" => "setup",
        "disable_dependencies_cache" => true
      })

      section_id = insert_section(session.pid)

      cell_id =
        insert_text_cell(session.pid, section_id, :code, ~s/System.get_env("MIX_INSTALL_FORCE")/)

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, "\e[32m\"true\"\e[0m"},
                       _}}
    end

    test "cancelling cell evaluation", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code, "Process.sleep(2000)")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      view
      |> element(~s{[data-el-session]})
      |> render_hook("cancel_cell_evaluation", %{"cell_id" => cell_id})

      assert %{cell_infos: %{^cell_id => %{eval: %{status: :ready}}}} =
               Session.get_data(session.pid)
    end

    test "setting cell to always reevaluating", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("#cell-#{cell_id}-evaluation-menu button", "Reevaluate automatically")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%Cell.Code{reevaluate_automatically: true}]}]}} =
               Session.get_data(session.pid)
    end

    test "inserting a cell below the given cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("insert_cell_below", %{"cell_id" => cell_id, "type" => "markdown"})

      assert %{notebook: %{sections: [%{cells: [_first_cell, %Cell.Markdown{}]}]}} =
               Session.get_data(session.pid)
    end

    test "inserting a cell at section start", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      _cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("insert_cell_below", %{"section_id" => section_id, "type" => "markdown"})

      assert %{notebook: %{sections: [%{cells: [%Cell.Markdown{}, _first_cell]}]}} =
               Session.get_data(session.pid)
    end

    test "deleting the given cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("delete_cell", %{"cell_id" => cell_id})

      assert %{notebook: %{sections: [%{cells: []}]}} = Session.get_data(session.pid)
    end

    test "restoring a deleted cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.delete_cell(session.pid, cell_id)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      refute render(view) =~ "cell-" <> cell_id

      view
      |> element(~s{a[aria-label="Bin (sb)"]})
      |> render_click()

      view
      |> element("button", "Restore")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%{id: ^cell_id}]}]}} =
               Session.get_data(session.pid)
    end

    test "editing input field in cell output", %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      input = %{
        ref: :input_ref,
        id: "input1",
        type: :number,
        label: "Name",
        default: "hey",
        destination: test
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, {:input, input})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s/[data-el-outputs-container] form/)
      |> render_change(%{"value" => "10"})

      assert %{input_values: %{"input1" => 10}} = Session.get_data(session.pid)

      assert_receive {:event, :input_ref, %{value: 10, type: :change}}
    end

    test "newlines in text input are normalized", %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      input = %{
        ref: :input_ref,
        id: "input1",
        type: :textarea,
        label: "Name",
        default: "hey",
        destination: test
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, {:input, input})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s/[data-el-outputs-container] form/)
      |> render_change(%{"value" => "line\r\nline"})

      assert %{input_values: %{"input1" => "line\nline"}} = Session.get_data(session.pid)
    end

    test "form input changes are reflected only in local LV data",
         %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      form_control = %{
        type: :form,
        ref: :form_ref,
        destination: test,
        fields: [
          name: %{
            ref: :input_ref,
            id: "input1",
            type: :text,
            label: "Name",
            default: "initial",
            destination: test
          }
        ],
        submit: "Send",
        report_changes: %{},
        reset_on_submit: []
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, {:control, form_control})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s/[data-el-outputs-container] form/)
      |> render_change(%{"value" => "sherlock"})

      # The new value is on the page
      assert render(view) =~ "sherlock"
      # but it's not reflected in the synchronized session data
      assert %{input_values: %{"input1" => "initial"}} = Session.get_data(session.pid)

      view
      |> element(~s/[data-el-outputs-container] button/, "Send")
      |> render_click()

      assert_receive {:event, :form_ref, %{data: %{name: "sherlock"}, type: :submit}}
    end

    test "file input", %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      input = %{
        ref: :input_ref,
        id: "input1",
        type: :file,
        label: "File",
        default: nil,
        destination: test,
        accept: :any
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, {:input, input})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> file_input(~s/[data-el-outputs-container] form/, :file, [
        %{
          last_modified: 1_594_171_879_000,
          name: "data.txt",
          content: "content",
          size: 7,
          type: "text/plain"
        }
      ])
      |> render_upload("data.txt")

      assert %{input_values: %{"input1" => value}} = Session.get_data(session.pid)

      assert %{file_ref: file_ref, client_name: "data.txt"} = value

      send(session.pid, {:runtime_file_lookup, self(), file_ref})
      assert_receive {:runtime_file_lookup_reply, {:ok, path}}
      assert File.read!(path) == "content"
    end
  end

  describe "outputs" do
    test "stdout output update", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      send(session.pid, {:runtime_evaluation_output, cell_id, {:stdout, "line 1\n"}})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      assert render(view) =~ "line 1"

      send(session.pid, {:runtime_evaluation_output, cell_id, {:stdout, "line 2\n"}})
      wait_for_session_update(session.pid)
      # Render once, so that stdout send_update is processed
      _ = render(view)
      assert render(view) =~ "line 2"
    end

    test "frame output update", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      send(
        session.pid,
        {:runtime_evaluation_output, cell_id,
         {:frame, [{:text, "In frame"}], %{ref: "1", type: :default}}}
      )

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      assert render(view) =~ "In frame"

      send(
        session.pid,
        {:runtime_evaluation_output, cell_id,
         {:frame, [{:text, "Updated frame"}], %{ref: "1", type: :replace}}}
      )

      wait_for_session_update(session.pid)

      # Render once, so that frame send_update is processed
      _ = render(view)

      content = render(view)
      assert content =~ "Updated frame"
      refute content =~ "In frame"
    end

    test "client-specific output is sent only to one target", %{conn: conn, session: session} do
      user1 = build(:user, name: "Jake Peralta")
      {_, client_id} = Session.register_client(session.pid, self(), user1)

      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      send(
        session.pid,
        {:runtime_evaluation_output_to, client_id, cell_id, {:stdout, "line 1\n"}}
      )

      assert_receive {:operation,
                      {:add_cell_evaluation_output, _, ^cell_id, {:stdout, "line 1\n"}}}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      refute render(view) =~ "line 1"
    end
  end

  describe "smart cells" do
    test "shows a new cell insert button when a new smart cell kind becomes available",
         %{conn: conn, session: session} do
      insert_section(session.pid)

      {:ok, runtime} = Runtime.Embedded.new() |> Runtime.connect()
      Session.set_runtime(session.pid, runtime)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      refute render(view) =~ "Database connection"

      send(
        session.pid,
        {:runtime_smart_cell_definitions,
         [%{kind: "dbconn", name: "Database connection", requirement: nil}]}
      )

      wait_for_session_update(session.pid)

      assert render(view) =~ "Database connection"
    end
  end

  describe "runtime settings" do
    test "connecting to elixir standalone updates connect button to reconnect",
         %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      Session.subscribe(session.id)

      view
      |> element("button", "Elixir standalone")
      |> render_click()

      [elixir_standalone_view] = live_children(view)

      elixir_standalone_view
      |> element("button", "Connect")
      |> render_click()

      assert_receive {:operation, {:set_runtime, _pid, %Runtime.ElixirStandalone{} = runtime}}

      page = render(view)
      assert page =~ Atom.to_string(runtime.node)
      assert page =~ "Reconnect"
      assert page =~ "Disconnect"
    end
  end

  describe "persistence settings" do
    @tag :tmp_dir
    test "saving to file shows the newly created file in file selector",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/file")

      path = Path.join(tmp_dir, "notebook.livemd")

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: path})

      view
      |> element(~s{#persistence-modal button}, "Save")
      |> render_click()

      assert Session.get_data(session.pid).file

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/file")

      assert view
             |> element("button", "notebook.livemd")
             |> has_element?()

      view
      |> element(~s{#persistence-modal button}, "Stop saving")
      |> render_click()

      refute Session.get_data(session.pid).file
    end

    @tag :tmp_dir
    test "changing output persistence updates data",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/file")

      path = Path.join(tmp_dir, "notebook.livemd")

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: path})

      view
      |> element(~s{form[phx-change="set_options"]})
      |> render_change(%{persist_outputs: "true"})

      view
      |> element(~s{#persistence-modal button}, "Save")
      |> render_click()

      assert %{notebook: %{persist_outputs: true}} = Session.get_data(session.pid)
    end
  end

  describe "completion" do
    test "replies with nil completion reference when no runtime is started",
         %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code, "Process.sleep(10)")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("intellisense_request", %{
        "cell_id" => cell_id,
        "type" => "completion",
        "hint" => "System.ver",
        "editor_auto_completion" => false
      })

      assert_reply(view, %{"ref" => nil})
    end

    test "replies with completion reference and then sends asynchronous response",
         %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code, "Process.sleep(10)")

      {:ok, runtime} = Runtime.Embedded.new() |> Runtime.connect()
      Session.set_runtime(session.pid, runtime)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("intellisense_request", %{
        "cell_id" => cell_id,
        "type" => "completion",
        "hint" => "System.ver",
        "editor_auto_completion" => false
      })

      assert_reply(view, %{"ref" => ref})
      assert ref != nil

      assert_push_event(
        view,
        "intellisense_response",
        %{
          "ref" => ^ref,
          "response" => %{items: [%{label: "version/0"}]}
        },
        1000
      )
    end
  end

  test "forking the session", %{conn: conn, session: session} do
    Session.set_notebook_name(session.pid, "My notebook")
    wait_for_session_update(session.pid)

    {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

    assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
             result =
             view
             |> element("button", "Fork")
             |> render_click()

    {:ok, view, _} = follow_redirect(result, conn)
    assert render(view) =~ "My notebook - fork"

    close_session_by_id(session_id)
  end

  @tag :tmp_dir
  test "starring and unstarring the notebook", %{conn: conn, session: session, tmp_dir: tmp_dir} do
    notebook_path = Path.join(tmp_dir, "notebook.livemd")
    file = Livebook.FileSystem.File.local(notebook_path)
    Session.set_file(session.pid, file)

    Livebook.NotebookManager.subscribe_starred_notebooks()

    {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

    view
    |> element("button", "Star notebook")
    |> render_click()

    assert_receive {:starred_notebooks_updated, starred_notebooks}

    assert Enum.any?(starred_notebooks, &(&1.file == file))

    view
    |> element("button", "Unstar notebook")
    |> render_click()

    assert_receive {:starred_notebooks_updated, starred_notebooks}

    refute Enum.any?(starred_notebooks, &(&1.file == file))
  end

  describe "connected users" do
    test "lists connected users", %{conn: conn, session: session} do
      user1 = build(:user, name: "Jake Peralta")

      client_pid =
        spawn_link(fn ->
          receive do
            :stop -> :ok
          end
        end)

      Session.register_client(session.pid, client_pid, user1)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert render(view) =~ "Jake Peralta"

      send(client_pid, :stop)
    end

    test "updates users list whenever a user joins or leaves",
         %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.subscribe(session.id)

      user1 = build(:user, name: "Jake Peralta")

      client_pid =
        spawn_link(fn ->
          receive do
            :stop -> :ok
          end
        end)

      {_, client_id} = Session.register_client(session.pid, client_pid, user1)

      assert_receive {:operation, {:client_join, ^client_id, _user}}
      assert render(view) =~ "Jake Peralta"

      send(client_pid, :stop)
      assert_receive {:operation, {:client_leave, ^client_id}}
      refute render(view) =~ "Jake Peralta"
    end

    test "updates users list whenever a user changes his data",
         %{conn: conn, session: session} do
      user1 = build(:user, name: "Jake Peralta")

      client_pid =
        spawn_link(fn ->
          receive do
            :stop -> :ok
          end
        end)

      Session.register_client(session.pid, client_pid, user1)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.subscribe(session.id)

      assert render(view) =~ "Jake Peralta"

      Users.broadcast_change(%{user1 | name: "Raymond Holt"})
      assert_receive {:operation, {:update_user, _client_id, _user}}

      refute render(view) =~ "Jake Peralta"
      assert render(view) =~ "Raymond Holt"

      send(client_pid, :stop)
    end
  end

  describe "relative paths" do
    test "renders an info message when the path doesn't have notebook extension",
         %{conn: conn, session: session} do
      session_path = "/sessions/#{session.id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/document.pdf")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Got unrecognised session path: document.pdf"
    end

    test "renders an info message when the session has neither original url nor path",
         %{conn: conn, session: session} do
      session_path = "/sessions/#{session.id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot resolve notebook path notebook.livemd, because the current notebook has no location"
    end

    @tag :tmp_dir
    test "renders an error message when the relative notebook does not exist",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      index_file = FileSystem.File.resolve(tmp_dir, "index.livemd")
      notebook_file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      Session.set_file(session.pid, index_file)
      wait_for_session_update(session.pid)

      session_path = "/sessions/#{session.id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot navigate, failed to read #{notebook_file.path}, reason: no such file or directory"
    end

    @tag :tmp_dir
    test "opens a relative notebook if it exists",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      index_file = FileSystem.File.resolve(tmp_dir, "index.livemd")
      notebook_file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      Session.set_file(session.pid, index_file)
      wait_for_session_update(session.pid)

      :ok = FileSystem.File.write(notebook_file, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: new_session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Sibling notebook"

      "/sessions/" <> session_id = new_session_path
      {:ok, session} = Sessions.fetch_session(session_id)
      data = Session.get_data(session.pid)
      assert data.file == notebook_file

      Session.close(session.pid)
    end

    @tag :tmp_dir
    test "if the current session has no path, forks the relative notebook",
         %{conn: conn, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      index_file = FileSystem.File.resolve(tmp_dir, "index.livemd")
      notebook_file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      {:ok, session} = Sessions.create_session(origin: {:file, index_file})

      :ok = FileSystem.File.write(notebook_file, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: new_session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Sibling notebook - fork"

      "/sessions/" <> session_id = new_session_path
      {:ok, new_session} = Sessions.fetch_session(session_id)
      data = Session.get_data(new_session.pid)
      assert data.file == nil
      assert data.origin == {:file, notebook_file}

      Session.close([session.pid, new_session.pid])
    end

    @tag :tmp_dir
    test "if the notebook is already open, redirects to the session",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      index_file = FileSystem.File.resolve(tmp_dir, "index.livemd")
      notebook_file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      Session.set_file(session.pid, index_file)
      wait_for_session_update(session.pid)

      :ok = FileSystem.File.write(notebook_file, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: session_path}}} =
               live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      "/sessions/" <> session_id = session_path
      close_session_by_id(session_id)
    end

    @tag :tmp_dir
    test "handles nested paths", %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      parent_file = FileSystem.File.resolve(tmp_dir, "parent.livemd")
      child_dir = FileSystem.File.resolve(tmp_dir, "dir/")
      child_file = FileSystem.File.resolve(child_dir, "child.livemd")

      Session.set_file(session.pid, parent_file)
      wait_for_session_update(session.pid)

      :ok = FileSystem.File.write(child_file, "# Child notebook")

      assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
               result = live(conn, ~p"/sessions/#{session.id}/dir/child.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Child notebook"

      close_session_by_id(session_id)
    end

    @tag :tmp_dir
    test "handles parent paths", %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      parent_file = FileSystem.File.resolve(tmp_dir, "parent.livemd")
      child_dir = FileSystem.File.resolve(tmp_dir, "dir/")
      child_file = FileSystem.File.resolve(child_dir, "child.livemd")

      Session.set_file(session.pid, child_file)
      wait_for_session_update(session.pid)

      :ok = FileSystem.File.write(parent_file, "# Parent notebook")

      assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
               result = live(conn, ~p"/sessions/#{session.id}/__parent__/parent.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Parent notebook"

      close_session_by_id(session_id)
    end

    test "resolves remote URLs", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, "# My notebook")
      end)

      index_url = bypass_url(bypass.port) <> "/index.livemd"
      {:ok, session} = Sessions.create_session(origin: {:url, index_url})

      assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "My notebook"

      Session.close(session.pid)
      close_session_by_id(session_id)
    end

    test "when a remote URL cannot be loaded, attempts to resolve a flat URL", %{conn: conn} do
      bypass = Bypass.open()

      # Multi-level path is not available
      Bypass.expect_once(bypass, "GET", "/nested/path/to/notebook.livemd", fn conn ->
        Plug.Conn.resp(conn, 500, "Error")
      end)

      # A flat path is available
      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, "# My notebook")
      end)

      index_url = bypass_url(bypass.port) <> "/index.livemd"
      {:ok, session} = Sessions.create_session(origin: {:url, index_url})

      assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
               result = live(conn, ~p"/sessions/#{session.id}/nested/path/to/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "My notebook"

      Session.close(session.pid)
      close_session_by_id(session_id)
    end

    test "renders an error message if relative remote notebook cannot be loaded", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        Plug.Conn.resp(conn, 500, "Error")
      end)

      index_url = bypass_url(bypass.port) <> "/index.livemd"

      {:ok, session} = Sessions.create_session(origin: {:url, index_url})

      session_path = "/sessions/#{session.id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Cannot navigate, failed to download notebook from the given URL"

      Session.close(session.pid)
    end

    test "if the remote notebook is already imported, redirects to the session",
         %{conn: conn, test: test} do
      test_path = test |> to_string() |> URI.encode_www_form()
      index_url = "http://example.com/#{test_path}/index.livemd"
      notebook_url = "http://example.com/#{test_path}/notebook.livemd"

      {:ok, index_session} = Sessions.create_session(origin: {:url, index_url})
      {:ok, notebook_session} = Sessions.create_session(origin: {:url, notebook_url})

      notebook_session_path = "/sessions/#{notebook_session.id}"

      assert {:error, {:live_redirect, %{to: ^notebook_session_path}}} =
               live(conn, ~p"/sessions/#{index_session.id}/notebook.livemd")

      Session.close([index_session.pid, notebook_session.pid])
    end

    test "renders an error message if there are already multiple session imported from the relative URL",
         %{conn: conn, test: test} do
      test_path = test |> to_string() |> URI.encode_www_form()
      index_url = "http://example.com/#{test_path}/index.livemd"
      notebook_url = "http://example.com/#{test_path}/notebook.livemd"

      {:ok, index_session} = Sessions.create_session(origin: {:url, index_url})
      {:ok, notebook_session1} = Sessions.create_session(origin: {:url, notebook_url})
      {:ok, notebook_session2} = Sessions.create_session(origin: {:url, notebook_url})

      index_session_path = "/sessions/#{index_session.id}"

      assert {:error, {:live_redirect, %{to: ^index_session_path}}} =
               result = live(conn, ~p"/sessions/#{index_session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot navigate, because multiple sessions were found for #{notebook_url}"

      Session.close([index_session.pid, notebook_session1.pid, notebook_session2.pid])
    end
  end

  describe "package search" do
    test "lists search entries", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/package-search")

      [search_view] = live_children(view)

      # Search the predefined dependencies in the embedded runtime
      search_view
      |> element(~s{form[phx-change="search"]})
      |> render_change(%{"search" => "ja"})

      page = render(view)
      assert page =~ "jason"
      assert page =~ "A blazing fast JSON parser and generator in pure Elixir"
      assert page =~ "1.3.0"
    end
  end

  describe "secrets" do
    setup do
      {:ok, hub: build(:personal)}
    end

    test "adds a secret from form", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")
      secret = build(:secret, name: "FOO", value: "123", hub_id: nil)

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}})

      assert_session_secret(view, session.pid, secret)
    end

    test "adds a livebook secret from form", %{conn: conn, session: session, hub: hub} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")
      secret = build(:secret, name: "BAR", value: "456")

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{
        secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}
      })

      assert secret in Livebook.Hubs.get_secrets(hub)
    end

    test "syncs secrets", %{conn: conn, session: session, hub: hub} do
      session_secret = insert_secret(name: "FOO", value: "123")
      secret = build(:secret, name: "FOO", value: "456")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{
        secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}
      })

      assert_session_secret(view, session.pid, secret)
      assert secret in Livebook.Hubs.get_secrets(hub)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")
      Session.set_secret(session.pid, session_secret)

      secret = build(:secret, name: "FOO", value: "789")

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{
        secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}
      })

      assert_session_secret(view, session.pid, secret)
      assert secret in Livebook.Hubs.get_secrets(hub)
    end

    test "never syncs secrets when updating from session",
         %{conn: conn, session: session, hub: hub} do
      hub_secret = insert_secret(name: "FOO", value: "123")
      secret = build(:secret, name: "FOO", value: "456", hub_id: nil)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")
      Session.set_secret(session.pid, hub_secret)

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}})

      assert_session_secret(view, session.pid, secret)
      refute secret in Livebook.Hubs.get_secrets(hub)
      assert hub_secret in Livebook.Hubs.get_secrets(hub)
    end

    test "shows the 'Add secret' button for missing secrets", %{conn: conn, session: session} do
      secret = build(:secret, name: "ANOTHER_GREAT_SECRET", value: "123456", hub_id: nil)
      Session.subscribe(session.id)
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert view
             |> element("a", "Add secret")
             |> has_element?()
    end

    test "adding a missing secret using 'Add secret' button",
         %{conn: conn, session: session, hub: hub} do
      secret = build(:secret, name: "MYUNAVAILABLESECRET", value: "123456", hub_id: nil)

      # Subscribe and executes the code to trigger
      # the `System.EnvError` exception and outputs the 'Add secret' button
      Session.subscribe(session.id)
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      # Enters the session to check if the button exists
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      expected_url = ~p"/sessions/#{session.id}/secrets?secret_name=#{secret.name}"
      add_secret_button = element(view, "a[href='#{expected_url}']")
      assert has_element?(add_secret_button)

      # Clicks the button and fills the form to create a new secret
      # that prefilled the name with the received from exception.
      render_click(add_secret_button)
      secrets_component = with_target(view, "#secrets-modal")
      form_element = element(secrets_component, "form[phx-submit='save']")
      assert has_element?(form_element)
      render_submit(form_element, %{secret: %{value: secret.value, hub_id: secret.hub_id}})

      # Checks if the secret isn't an app secret
      refute secret in Livebook.Hubs.get_secrets(hub)

      # Checks if the secret exists and is inside the session,
      # then executes the code cell again and checks if the
      # secret value is what we expected.
      assert_session_secret(view, session.pid, secret)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, output}, _}}

      assert output == "\e[32m\"#{secret.value}\"\e[0m"
    end

    test "granting access for unavailable secret using 'Add secret' button",
         %{conn: conn, session: session, hub: hub} do
      secret = insert_secret(name: "UNAVAILABLESECRET", value: "123456")

      # Subscribe and executes the code to trigger
      # the `System.EnvError` exception and outputs the 'Add secret' button
      Session.subscribe(session.id)
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      # Enters the session to check if the button exists
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      expected_url = ~p"/sessions/#{session.id}/secrets?secret_name=#{secret.name}"
      add_secret_button = element(view, "a[href='#{expected_url}']")
      assert has_element?(add_secret_button)

      # Checks if the secret is persisted
      assert secret in Livebook.Hubs.get_secrets(hub)

      # Clicks the button and checks if the 'Grant access' banner
      # is being shown, so clicks it's button to set the app secret
      # to the session, allowing the user to fetches the secret.
      render_click(add_secret_button)
      secrets_component = with_target(view, "#secrets-modal")

      assert render(secrets_component) =~
               "in #{hub_label(secret)}. Allow this session to access it?"

      grant_access_button = element(secrets_component, "button", "Grant access")
      render_click(grant_access_button)

      # Checks if the secret exists and is inside the session,
      # then executes the code cell again and checks if the
      # secret value is what we expected.
      assert_session_secret(view, session.pid, secret)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, output}, _}}

      assert output == "\e[32m\"#{secret.value}\"\e[0m"
    end

    test "reloading outdated secret value", %{conn: conn, session: session} do
      hub_secret = insert_secret(name: "FOO", value: "123")
      Session.set_secret(session.pid, hub_secret)

      {:ok, updated_hub_secret} = Livebook.Secrets.update_secret(hub_secret, %{value: "456"})
      hub = Livebook.Hubs.fetch_hub!(hub_secret.hub_id)
      :ok = Livebook.Hubs.update_secret(hub, updated_hub_secret)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")

      assert_session_secret(view, session.pid, hub_secret)

      view
      |> element(~s{button[aria-label="load latest value"]})
      |> render_click()

      assert_session_secret(view, session.pid, updated_hub_secret)
    end
  end

  describe "environment variables" do
    test "outputs persisted env var from ets", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      section_id = insert_section(session.pid)

      cell_id =
        insert_text_cell(session.pid, section_id, :code, ~s{System.get_env("MY_AWESOME_ENV")})

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, "\e[35mnil\e[0m"}, _}}

      attrs = params_for(:env_var, name: "MY_AWESOME_ENV", value: "MyEnvVarValue")
      Settings.set_env_var(attrs)

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       {:text, "\e[32m\"MyEnvVarValue\"\e[0m"}, _}}

      Settings.set_env_var(%{attrs | value: "OTHER_VALUE"})

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       {:text, "\e[32m\"OTHER_VALUE\"\e[0m"}, _}}

      Settings.unset_env_var("MY_AWESOME_ENV")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, "\e[35mnil\e[0m"}, _}}
    end

    @tag :tmp_dir
    test "outputs persisted PATH delimited with os PATH env var",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      # Start the standalone runtime, to encapsulate env var changes
      {:ok, runtime} = Runtime.ElixirStandalone.new() |> Runtime.connect()
      Session.set_runtime(session.pid, runtime)

      separator =
        case :os.type() do
          {:win32, _} -> ";"
          _ -> ":"
        end

      initial_os_path = System.get_env("PATH", "")
      expected_path = initial_os_path <> separator <> tmp_dir

      attrs = params_for(:env_var, name: "PATH", value: tmp_dir)
      Settings.set_env_var(attrs)

      Session.subscribe(session.id)
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      section_id = insert_section(session.pid)

      cell_id = insert_text_cell(session.pid, section_id, :code, ~s{System.get_env("PATH")})

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, output}, _}}

      assert output == "\e[32m\"#{String.replace(expected_path, "\\", "\\\\")}\"\e[0m"

      Settings.unset_env_var("PATH")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, output}, _}}

      assert output == "\e[32m\"#{String.replace(initial_os_path, "\\", "\\\\")}\"\e[0m"
    end
  end

  describe "apps" do
    test "deploying an app", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      section_id = insert_section(session.pid)

      insert_cell_with_output(session.pid, section_id, {:markdown, "Hello from the app!"})

      slug = Livebook.Utils.random_short_id()

      view
      |> element(~s/[data-el-app-info] form/)
      |> render_submit(%{"app_settings" => %{"slug" => slug}})

      assert_receive {:operation, {:add_app, _, _, app_session_pid}}
      assert_receive {:operation, {:set_app_registered, _, _, true}}

      assert render(view) =~ "/apps/#{slug}"

      {:ok, view, _} = live(conn, ~p"/apps/#{slug}")

      assert_push_event(view, "markdown_renderer:" <> _, %{content: "Hello from the app!"})

      Session.app_unregistered(app_session_pid)
    end

    test "stopping and terminating an app", %{conn: conn, session: session} do
      Session.subscribe(session.id)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)
      Session.deploy_app(session.pid)

      assert_receive {:operation, {:add_app, _, _, _}}
      assert_receive {:operation, {:set_app_registered, _, _, true}}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s/[data-el-app-info] button[aria-label="stop app"]/)
      |> render_click()

      assert_receive {:operation, {:set_app_registered, _, _, false}}

      view
      |> element(~s/[data-el-app-info] button[aria-label="terminate app"]/)
      |> render_click()

      assert_receive {:operation, {:delete_app, _, _}}

      refute render(view) =~ "/apps/#{slug}"
    end
  end

  describe "hubs" do
    test "selects the notebook hub", %{conn: conn, session: session} do
      hub = insert_hub(:fly)
      id = hub.id
      personal_id = Livebook.Hubs.Personal.id()

      Session.subscribe(session.id)
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert Session.get_data(session.pid).notebook.hub_id == personal_id

      view
      |> element(~s/#select-hub-#{id}/)
      |> render_click()

      assert_receive {:operation, {:set_notebook_hub, _, ^id}}
      assert Session.get_data(session.pid).notebook.hub_id == hub.id
    end
  end
end
