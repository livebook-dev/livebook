defmodule LivebookWeb.AppLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.{App, Apps, Notebook, Utils}

  describe "single-session app" do
    test "redirects to the current session page", %{conn: conn} do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: false}
      notebook = %{Notebook.new() | app_settings: app_settings}

      Apps.subscribe()
      {:ok, app_pid} = Apps.deploy(notebook)

      assert_receive {:app_created, %{pid: ^app_pid, sessions: [%{id: session_id}]}}

      {:error, {:live_redirect, %{to: to}}} = live(conn, ~p"/apps/#{slug}")
      assert to == ~p"/apps/#{slug}/#{session_id}"

      App.close(app_pid)
    end
  end

  describe "multi-session app" do
    test "renders a list of active app sessions", %{conn: conn} do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}
      notebook = %{Notebook.new() | app_settings: app_settings}

      Apps.subscribe()
      {:ok, app_pid} = Apps.deploy(notebook)

      assert_receive {:app_created, %{pid: ^app_pid, sessions: []}}

      session_id1 = App.get_session_id(app_pid)
      session_id2 = App.get_session_id(app_pid)

      assert_receive {:app_updated,
                      %{pid: ^app_pid, sessions: [%{id: ^session_id2, pid: session_pid2}, _]}}

      Livebook.Session.app_deactivate(session_pid2)

      assert_receive {:app_updated,
                      %{
                        pid: ^app_pid,
                        sessions: [%{app_status: %{lifecycle: :deactivated}}, _]
                      }}

      {:ok, view, _} = live(conn, ~p"/apps/#{slug}")

      assert render(view) =~ ~p"/apps/#{slug}/#{session_id1}"
      refute render(view) =~ ~p"/apps/#{slug}/#{session_id2}"

      # Create a new app session
      session_id3 = App.get_session_id(app_pid)

      assert_receive {:app_updated,
                      %{pid: ^app_pid, sessions: [%{id: ^session_id3, pid: session_pid3}, _, _]}}

      assert render(view) =~ ~p"/apps/#{slug}/#{session_id3}"

      # Deactivate the app session
      Livebook.Session.app_deactivate(session_pid3)

      assert_receive {:app_updated,
                      %{
                        pid: ^app_pid,
                        sessions: [%{app_status: %{lifecycle: :deactivated}}, _, _]
                      }}

      assert render(view) =~ ~p"/apps/#{slug}/#{session_id1}"
      refute render(view) =~ ~p"/apps/#{slug}/#{session_id3}"

      App.close(app_pid)
    end

    test "creating a new session", %{conn: conn} do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}
      notebook = %{Notebook.new() | app_settings: app_settings}

      Apps.subscribe()
      {:ok, app_pid} = Apps.deploy(notebook)

      assert_receive {:app_created, %{pid: ^app_pid, sessions: []}}

      {:ok, view, _} = live(conn, ~p"/apps/#{slug}")

      {:error, {:live_redirect, %{to: to}}} =
        view
        |> element("a", "New session")
        |> render_click()

      assert_receive {:app_updated, %{pid: ^app_pid, sessions: [%{id: session_id}]}}

      assert to == ~p"/apps/#{slug}/#{session_id}"

      App.close(app_pid)
    end

    test "does not list existing session if configured not to", %{conn: conn} do
      slug = Utils.random_short_id()

      app_settings = %{
        Notebook.AppSettings.new()
        | slug: slug,
          multi_session: true,
          show_existing_sessions: false
      }

      notebook = %{Notebook.new() | app_settings: app_settings}

      Apps.subscribe()

      {:ok, app_pid} = Apps.deploy(notebook)
      session_id = App.get_session_id(app_pid)
      assert_receive {:app_updated, %{pid: ^app_pid, sessions: [%{id: ^session_id}]}}

      {:ok, view, _} = live(conn, ~p"/apps/#{slug}")

      refute render(view) =~ session_id

      App.close(app_pid)
    end
  end
end
