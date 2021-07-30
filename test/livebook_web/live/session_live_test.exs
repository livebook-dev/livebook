defmodule LivebookWeb.SessionLiveTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Livebook.{SessionSupervisor, Session, Delta, Runtime, Users}
  alias Livebook.Notebook.Cell
  alias Livebook.Users.User

  setup do
    {:ok, session_id} = SessionSupervisor.create_session()
    %{session_id: session_id}
  end

  test "disconnected and connected render", %{conn: conn, session_id: session_id} do
    {:ok, view, disconnected_html} = live(conn, "/sessions/#{session_id}")
    assert disconnected_html =~ "Untitled notebook"
    assert render(view) =~ "Untitled notebook"
  end

  describe "asynchronous updates" do
    test "renders an updated notebook name", %{conn: conn, session_id: session_id} do
      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      Session.set_notebook_name(session_id, "My notebook")
      wait_for_session_update(session_id)

      assert render(view) =~ "My notebook"
    end

    test "renders a newly inserted section", %{conn: conn, session_id: session_id} do
      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      section_id = insert_section(session_id)

      assert render(view) =~ section_id
    end

    test "renders an updated section name", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      Session.set_section_name(session_id, section_id, "My section")
      wait_for_session_update(session_id)

      assert render(view) =~ "My section"
    end

    test "renders a newly inserted cell", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      cell_id = insert_text_cell(session_id, section_id, :markdown)

      assert render(view) =~ cell_id
    end

    test "un-renders a deleted cell", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_text_cell(session_id, section_id, :markdown)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      Session.delete_cell(session_id, cell_id)
      wait_for_session_update(session_id)

      refute render(view) =~ cell_id
    end
  end

  describe "UI events update the shared state" do
    test "adding a new section", %{conn: conn, session_id: session_id} do
      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("button", "New section")
      |> render_click()

      assert %{notebook: %{sections: [_section]}} = Session.get_data(session_id)
    end

    test "adding a new cell", %{conn: conn, session_id: session_id} do
      Session.insert_section(session_id, 0)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("button", "+ Markdown")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%Cell.Markdown{}]}]}} =
               Session.get_data(session_id)
    end

    test "queueing cell evaluation", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_text_cell(session_id, section_id, :elixir, "Process.sleep(10)")

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert %{cell_infos: %{^cell_id => %{evaluation_status: :evaluating}}} =
               Session.get_data(session_id)
    end

    test "cancelling cell evaluation", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_text_cell(session_id, section_id, :elixir, "Process.sleep(2000)")

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      view
      |> element("#session")
      |> render_hook("cancel_cell_evaluation", %{"cell_id" => cell_id})

      assert %{cell_infos: %{^cell_id => %{evaluation_status: :ready}}} =
               Session.get_data(session_id)
    end

    test "inserting a cell below the given cell", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_text_cell(session_id, section_id, :elixir)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("insert_cell_below", %{"cell_id" => cell_id, "type" => "markdown"})

      assert %{notebook: %{sections: [%{cells: [_first_cell, %Cell.Markdown{}]}]}} =
               Session.get_data(session_id)
    end

    test "inserting a cell above the given cell", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_text_cell(session_id, section_id, :elixir)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("insert_cell_above", %{"cell_id" => cell_id, "type" => "markdown"})

      assert %{notebook: %{sections: [%{cells: [%Cell.Markdown{}, _first_cell]}]}} =
               Session.get_data(session_id)
    end

    test "deleting the given cell", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_text_cell(session_id, section_id, :elixir)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("delete_cell", %{"cell_id" => cell_id})

      assert %{notebook: %{sections: [%{cells: []}]}} = Session.get_data(session_id)
    end

    test "newlines in input values are normalized", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_input_cell(session_id, section_id)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element(~s/form[phx-change="set_cell_value"]/)
      |> render_change(%{"value" => "line\r\nline"})

      assert %{notebook: %{sections: [%{cells: [%{id: ^cell_id, value: "line\nline"}]}]}} =
               Session.get_data(session_id)
    end
  end

  describe "runtime settings" do
    test "connecting to elixir standalone updates connect button to reconnect",
         %{conn: conn, session_id: session_id} do
      {:ok, view, _} = live(conn, "/sessions/#{session_id}/settings/runtime")

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

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

  @tag :tmp_dir
  describe "persistence settings" do
    test "saving to file shows the newly created file",
         %{conn: conn, session_id: session_id, tmp_dir: tmp_dir} do
      {:ok, view, _} = live(conn, "/sessions/#{session_id}/settings/file")

      path = Path.join(tmp_dir, "notebook.livemd")

      view
      |> element("button", "Save to file")
      |> render_click()

      view
      |> element(~s{form[phx-change="set_path"]})
      |> render_change(%{path: path})

      view
      |> element(~s{button[phx-click="save"]}, "Save")
      |> render_click()

      assert view
             |> element("button", "notebook.livemd")
             |> has_element?()
    end

    test "changing output persistence updates data", %{conn: conn, session_id: session_id} do
      {:ok, view, _} = live(conn, "/sessions/#{session_id}/settings/file")

      view
      |> element("button", "Save to file")
      |> render_click()

      view
      |> element(~s{form[phx-change="set_options"]})
      |> render_change(%{persist_outputs: "true"})

      view
      |> element(~s{button[phx-click="save"]}, "Save")
      |> render_click()

      assert %{notebook: %{persist_outputs: true}} = Session.get_data(session_id)
    end
  end

  describe "completion" do
    test "replies with nil completion reference when no runtime is started",
         %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_text_cell(session_id, section_id, :elixir, "Process.sleep(10)")

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("intellisense_request", %{
        "cell_id" => cell_id,
        "type" => "completion",
        "hint" => "System.ver"
      })

      assert_reply view, %{"ref" => nil}
    end

    test "replies with completion reference and then sends asynchronous response",
         %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_text_cell(session_id, section_id, :elixir, "Process.sleep(10)")

      {:ok, runtime} = Livebook.Runtime.Embedded.init()
      Session.connect_runtime(session_id, runtime)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("intellisense_request", %{
        "cell_id" => cell_id,
        "type" => "completion",
        "hint" => "System.ver"
      })

      assert_reply view, %{"ref" => ref}
      assert ref != nil

      assert_push_event(view, "intellisense_response", %{
        "ref" => ^ref,
        "response" => %{items: [%{label: "version/0"}]}
      })
    end
  end

  test "forking the session", %{conn: conn, session_id: session_id} do
    Session.set_notebook_name(session_id, "My notebook")
    wait_for_session_update(session_id)

    {:ok, view, _} = live(conn, "/sessions/#{session_id}")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element("button", "Fork")
             |> render_click()

    assert to =~ "/sessions/"

    {:ok, view, _} = live(conn, to)
    assert render(view) =~ "My notebook - fork"
  end

  describe "connected users" do
    test "lists connected users", %{conn: conn, session_id: session_id} do
      user1 = create_user_with_name("Jake Peralta")

      client_pid =
        spawn_link(fn ->
          Session.register_client(session_id, self(), user1)

          receive do
            :stop -> :ok
          end
        end)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      assert render(view) =~ "Jake Peralta"

      send(client_pid, :stop)
    end

    test "updates users list whenever a user joins or leaves",
         %{conn: conn, session_id: session_id} do
      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

      user1 = create_user_with_name("Jake Peralta")

      client_pid =
        spawn_link(fn ->
          Session.register_client(session_id, self(), user1)

          receive do
            :stop -> :ok
          end
        end)

      assert_receive {:operation, {:client_join, ^client_pid, _user}}
      assert render(view) =~ "Jake Peralta"

      send(client_pid, :stop)
      assert_receive {:operation, {:client_leave, ^client_pid}}
      refute render(view) =~ "Jake Peralta"
    end

    test "updates users list whenever a user changes his data",
         %{conn: conn, session_id: session_id} do
      user1 = create_user_with_name("Jake Peralta")

      client_pid =
        spawn_link(fn ->
          Session.register_client(session_id, self(), user1)

          receive do
            :stop -> :ok
          end
        end)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

      assert render(view) =~ "Jake Peralta"

      Users.broadcast_change(%{user1 | name: "Raymond Holt"})
      assert_receive {:operation, {:update_user, _pid, _user}}

      refute render(view) =~ "Jake Peralta"
      assert render(view) =~ "Raymond Holt"

      send(client_pid, :stop)
    end
  end

  describe "input cell settings" do
    test "setting input cell attributes updates data", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_input_cell(session_id, section_id)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}/cell-settings/#{cell_id}")

      form_selector = ~s/[role="dialog"] form/

      assert view
             |> element(form_selector)
             |> render_change(%{attrs: %{type: "range"}}) =~
               ~s{<div class="input-label">Min</div>}

      view
      |> element(form_selector)
      |> render_change(%{attrs: %{name: "length"}})

      view
      |> element(form_selector)
      |> render_change(%{attrs: %{props: %{min: "10"}}})

      view
      |> element(form_selector)
      |> render_submit()

      assert %{
               notebook: %{
                 sections: [
                   %{
                     cells: [
                       %{
                         id: ^cell_id,
                         type: :range,
                         name: "length",
                         props: %{min: 10, max: 100, step: 1}
                       }
                     ]
                   }
                 ]
               }
             } = Session.get_data(session_id)
    end
  end

  describe "relative paths" do
    test "renders an info message when the path doesn't have notebook extension",
         %{conn: conn, session_id: session_id} do
      session_path = "/sessions/#{session_id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, "/sessions/#{session_id}/document.pdf")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Got unrecognised session path: document.pdf"
    end

    test "renders an info message when the session has neither original url nor path",
         %{conn: conn, session_id: session_id} do
      session_path = "/sessions/#{session_id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, "/sessions/#{session_id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot resolve notebook path notebook.livemd, because the current notebook has no location"
    end

    @tag :tmp_dir
    test "renders an error message when the relative notebook does not exist",
         %{conn: conn, session_id: session_id, tmp_dir: tmp_dir} do
      index_path = Path.join(tmp_dir, "index.livemd")
      notebook_path = Path.join(tmp_dir, "notebook.livemd")

      Session.set_path(session_id, index_path)
      wait_for_session_update(session_id)

      session_path = "/sessions/#{session_id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, "/sessions/#{session_id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot navigate, failed to read #{notebook_path}, reason: no such file or directory"
    end

    @tag :tmp_dir
    test "opens a relative notebook if it exists",
         %{conn: conn, session_id: session_id, tmp_dir: tmp_dir} do
      index_path = Path.join(tmp_dir, "index.livemd")
      notebook_path = Path.join(tmp_dir, "notebook.livemd")

      Session.set_path(session_id, index_path)
      wait_for_session_update(session_id)

      File.write!(notebook_path, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: new_session_path}}} =
               result = live(conn, "/sessions/#{session_id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Sibling notebook"

      "/sessions/" <> session_id = new_session_path
      data = Session.get_data(session_id)
      assert data.path == notebook_path
    end

    @tag :tmp_dir
    test "if the current session has no path, forks the relative notebook",
         %{conn: conn, tmp_dir: tmp_dir} do
      index_path = Path.join(tmp_dir, "index.livemd")
      notebook_path = Path.join(tmp_dir, "notebook.livemd")

      {:ok, session_id} = SessionSupervisor.create_session(origin_url: "file://" <> index_path)

      File.write!(notebook_path, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: new_session_path}}} =
               result = live(conn, "/sessions/#{session_id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Sibling notebook - fork"

      "/sessions/" <> session_id = new_session_path
      data = Session.get_data(session_id)
      assert data.path == nil
      assert data.origin_url == "file://" <> notebook_path
    end

    @tag :tmp_dir
    test "if the notebook is already open, redirects to the session",
         %{conn: conn, session_id: session_id, tmp_dir: tmp_dir} do
      index_path = Path.join(tmp_dir, "index.livemd")
      notebook_path = Path.join(tmp_dir, "notebook.livemd")

      Session.set_path(session_id, index_path)
      wait_for_session_update(session_id)

      File.write!(notebook_path, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: session_path}}} =
               live(conn, "/sessions/#{session_id}/notebook.livemd")

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               live(conn, "/sessions/#{session_id}/notebook.livemd")
    end

    @tag :tmp_dir
    test "handles nested paths", %{conn: conn, session_id: session_id, tmp_dir: tmp_dir} do
      parent_path = Path.join(tmp_dir, "parent.livemd")
      child_dir = Path.join(tmp_dir, "dir")
      child_path = Path.join(child_dir, "child.livemd")

      Session.set_path(session_id, parent_path)
      wait_for_session_update(session_id)

      File.mkdir!(child_dir)
      File.write!(child_path, "# Child notebook")

      {:ok, view, _} =
        conn
        |> live("/sessions/#{session_id}/dir/child.livemd")
        |> follow_redirect(conn)

      assert render(view) =~ "Child notebook"
    end

    @tag :tmp_dir
    test "handles parent paths", %{conn: conn, session_id: session_id, tmp_dir: tmp_dir} do
      parent_path = Path.join(tmp_dir, "parent.livemd")
      child_dir = Path.join(tmp_dir, "dir")
      child_path = Path.join(child_dir, "child.livemd")

      File.mkdir!(child_dir)
      Session.set_path(session_id, child_path)
      wait_for_session_update(session_id)

      File.write!(parent_path, "# Parent notebook")

      {:ok, view, _} =
        conn
        |> live("/sessions/#{session_id}/__parent__/parent.livemd")
        |> follow_redirect(conn)

      assert render(view) =~ "Parent notebook"
    end

    test "resolves remote URLs", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, "# My notebook")
      end)

      index_url = url(bypass.port) <> "/index.livemd"
      {:ok, session_id} = SessionSupervisor.create_session(origin_url: index_url)

      {:ok, view, _} =
        conn
        |> live("/sessions/#{session_id}/notebook.livemd")
        |> follow_redirect(conn)

      assert render(view) =~ "My notebook"
    end

    test "renders an error message if relative remote notebook cannot be loaded", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        Plug.Conn.resp(conn, 500, "Error")
      end)

      index_url = url(bypass.port) <> "/index.livemd"

      {:ok, session_id} = SessionSupervisor.create_session(origin_url: index_url)

      session_path = "/sessions/#{session_id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, "/sessions/#{session_id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Cannot navigate, failed to download notebook from the given URL"
    end

    test "if the remote notebook is already imported, redirects to the session",
         %{conn: conn, test: test} do
      index_url = "http://example.com/#{test}/index.livemd"
      notebook_url = "http://example.com/#{test}/notebook.livemd"

      {:ok, index_session_id} = SessionSupervisor.create_session(origin_url: index_url)
      {:ok, notebook_session_id} = SessionSupervisor.create_session(origin_url: notebook_url)

      notebook_session_path = "/sessions/#{notebook_session_id}"

      assert {:error, {:live_redirect, %{to: ^notebook_session_path}}} =
               live(conn, "/sessions/#{index_session_id}/notebook.livemd")
    end

    test "renders an error message if there are already multiple session imported from the relative URL",
         %{conn: conn, test: test} do
      index_url = "http://example.com/#{test}/index.livemd"
      notebook_url = "http://example.com/#{test}/notebook.livemd"

      {:ok, index_session_id} = SessionSupervisor.create_session(origin_url: index_url)
      {:ok, _notebook_session_id1} = SessionSupervisor.create_session(origin_url: notebook_url)
      {:ok, _notebook_session_id2} = SessionSupervisor.create_session(origin_url: notebook_url)

      index_session_path = "/sessions/#{index_session_id}"

      assert {:error, {:live_redirect, %{to: ^index_session_path}}} =
               result = live(conn, "/sessions/#{index_session_id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot navigate, because multiple sessions were found for #{notebook_url}"
    end
  end

  # Helpers

  defp wait_for_session_update(session_id) do
    # This call is synchronous, so it gives the session time
    # for handling the previously sent change messages.
    Session.get_data(session_id)
    :ok
  end

  # Utils for sending session requests, waiting for the change to be applied
  # and retrieving new ids if applicable.

  defp insert_section(session_id) do
    Session.insert_section(session_id, 0)
    %{notebook: %{sections: [section]}} = Session.get_data(session_id)
    section.id
  end

  defp insert_text_cell(session_id, section_id, type, content \\ "") do
    Session.insert_cell(session_id, section_id, 0, type)
    %{notebook: %{sections: [%{cells: [cell]}]}} = Session.get_data(session_id)

    # We need to register ourselves as a client to start submitting cell deltas
    user = Livebook.Users.User.new()
    Session.register_client(session_id, self(), user)

    delta = Delta.new(insert: content)
    Session.apply_cell_delta(session_id, cell.id, delta, 1)

    wait_for_session_update(session_id)

    cell.id
  end

  defp insert_input_cell(session_id, section_id) do
    Session.insert_cell(session_id, section_id, 0, :input)
    %{notebook: %{sections: [%{cells: [cell]}]}} = Session.get_data(session_id)
    cell.id
  end

  defp create_user_with_name(name) do
    {:ok, user} = User.new() |> User.change(%{"name" => name})
    user
  end

  defp url(port), do: "http://localhost:#{port}"
end
