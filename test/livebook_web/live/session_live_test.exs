defmodule LivebookWeb.SessionLiveTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Livebook.{SessionSupervisor, Session, Delta, Runtime, Users}
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

      cell_id = insert_cell(session_id, section_id, :markdown)

      assert render(view) =~ cell_id
    end

    test "un-renders a deleted cell", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :markdown)

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

      assert %{notebook: %{sections: [%{cells: [%{type: :markdown}]}]}} =
               Session.get_data(session_id)
    end

    test "queueing cell evaluation", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :elixir, "Process.sleep(10)")

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert %{cell_infos: %{^cell_id => %{evaluation_status: :evaluating}}} =
               Session.get_data(session_id)
    end

    test "cancelling cell evaluation", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :elixir, "Process.sleep(2000)")

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
      cell_id = insert_cell(session_id, section_id, :elixir)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("insert_cell_below", %{"cell_id" => cell_id, "type" => "markdown"})

      assert %{notebook: %{sections: [%{cells: [_first_cell, %{type: :markdown}]}]}} =
               Session.get_data(session_id)
    end

    test "inserting a cell above the given cell", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :elixir)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("insert_cell_above", %{"cell_id" => cell_id, "type" => "markdown"})

      assert %{notebook: %{sections: [%{cells: [%{type: :markdown}, _first_cell]}]}} =
               Session.get_data(session_id)
    end

    test "deleting the given cell", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :elixir)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("delete_cell", %{"cell_id" => cell_id})

      assert %{notebook: %{sections: [%{cells: []}]}} = Session.get_data(session_id)
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
      |> element("form")
      |> render_change(%{path: path})

      view
      |> element(~s{button[phx-click="save"]}, "Save")
      |> render_click()

      assert view
             |> element("button", "notebook.livemd")
             |> has_element?()
    end
  end

  describe "completion" do
    test "replies with nil completion reference when no runtime is started",
         %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :elixir, "Process.sleep(10)")

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("completion_request", %{"cell_id" => cell_id, "hint" => "System.ver"})

      assert_reply view, %{"completion_ref" => nil}
    end

    test "replies with completion reference and then sends asynchronous response",
         %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :elixir, "Process.sleep(10)")

      {:ok, runtime} = Livebook.Runtime.Embedded.init()
      Session.connect_runtime(session_id, runtime)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("completion_request", %{"cell_id" => cell_id, "hint" => "System.ver"})

      assert_reply view, %{"completion_ref" => ref}
      assert ref != nil

      assert_push_event(view, "completion_response", %{
        "completion_ref" => ^ref,
        "items" => [%{label: "version/0"}]
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

  defp insert_cell(session_id, section_id, type, content \\ "") do
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

  defp create_user_with_name(name) do
    {:ok, user} = User.new() |> User.change(%{"name" => name})
    user
  end
end
