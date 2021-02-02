defmodule LiveBookWeb.SessionLiveTest do
  use LiveBookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias LiveBook.{SessionSupervisor, Session, Delta}

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

  describe "UI-triggered updates" do
    test "adding a new session updates the shared state", %{conn: conn, session_id: session_id} do
      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("button", "New section")
      |> render_click()

      assert %{notebook: %{sections: [_section]}} = Session.get_data(session_id)
    end

    test "adding a new cell updates the shared state", %{conn: conn, session_id: session_id} do
      Session.insert_section(session_id, 0)

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("button", "+ Markdown")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%{type: :markdown}]}]}} =
               Session.get_data(session_id)
    end

    test "queueing cell evaluation updates the shared state", %{conn: conn, session_id: session_id} do
      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :elixir, "Process.sleep(5)")

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert %{cell_infos: %{^cell_id => %{evaluation_status: :evaluating}}} =
               Session.get_data(session_id)
    end

    test "queueing cell evaluation defaults to the focused cell if no cell id is given",
         %{conn: conn, session_id: session_id} do

      section_id = insert_section(session_id)
      cell_id = insert_cell(session_id, section_id, :elixir, "Process.sleep(5)")

      {:ok, view, _} = live(conn, "/sessions/#{session_id}")

      view
      |> element("#session")
      |> render_hook("focus_cell", %{"cell_id" => cell_id})

      view
      |> element("#session")
      |> render_hook("queue_cell_evaluation", %{})

      assert %{cell_infos: %{^cell_id => %{evaluation_status: :evaluating}}} =
               Session.get_data(session_id)
    end
  end

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

    delta = Delta.new(insert: content)
    Session.apply_cell_delta(session_id, self(), cell.id, delta, 1)

    cell.id
  end
end
