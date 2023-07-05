defmodule Livebook.AppTest do
  use ExUnit.Case, async: true

  alias Livebook.{App, Notebook, Utils}

  describe "start_link/1" do
    test "eagerly starts a session in single-session mode" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug}
      notebook = %{Notebook.new() | app_settings: app_settings}

      assert {:ok, app_pid} = App.start_link(notebook: notebook)
      assert %{sessions: [%{pid: session_pid}]} = App.get_by_pid(app_pid)

      assert %{mode: :app} = Livebook.Session.get_by_pid(session_pid)
    end

    test "does not eagerly start session when auto shutdown on inactivity is configured" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, auto_shutdown_ms: 5_000}
      notebook = %{Notebook.new() | app_settings: app_settings}

      assert {:ok, app_pid} = App.start_link(notebook: notebook)
      assert %{sessions: []} = App.get_by_pid(app_pid)
    end

    test "does not eagerly start session in multi-session mode" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}
      notebook = %{Notebook.new() | app_settings: app_settings}

      assert {:ok, app_pid} = App.start_link(notebook: notebook)
      assert %{sessions: []} = App.get_by_pid(app_pid)
    end
  end

  describe "deploy/2" do
    test "updates app version and reflects new notebook name" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug}
      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      App.deploy(app_pid, %{notebook | name: "New name"})
      assert %{version: 2, notebook_name: "New name"} = App.get_by_pid(app_pid)
    end

    test "keeps old sessions in multi-session mode" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}
      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      session_id = App.get_session_id(app_pid)

      App.subscribe(slug)

      App.deploy(app_pid, notebook)

      assert_receive {:app_updated,
                      %{
                        version: 2,
                        sessions: [
                          %{id: ^session_id, app_status: %{execution: :executed}, version: 1}
                        ]
                      }}
    end

    test "shuts down old sessions in single-session mode" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug}
      notebook = %{Notebook.new() | app_settings: app_settings}

      App.subscribe(slug)

      app_pid = start_app(notebook)

      assert_receive {:app_updated,
                      %{sessions: [%{app_status: %{execution: :executed}, version: 1}]}}

      App.deploy(app_pid, notebook)

      assert_receive {:app_updated,
                      %{
                        sessions: [
                          %{version: 2},
                          %{app_status: %{lifecycle: :shutting_down}, version: 1}
                        ]
                      }}

      assert_receive {:app_updated,
                      %{sessions: [%{app_status: %{execution: :executed}, version: 2}]}}
    end

    test "keeps old executed session during single-session zero-downtime deployment" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, zero_downtime: true}
      notebook = %{Notebook.new() | app_settings: app_settings}

      App.subscribe(slug)

      app_pid = start_app(notebook)

      assert_receive {:app_updated,
                      %{sessions: [%{app_status: %{execution: :executed}, version: 1}]}}

      App.deploy(app_pid, notebook)

      assert_receive {:app_updated,
                      %{
                        sessions: [
                          %{app_status: %{execution: :executing}, version: 2},
                          %{app_status: %{execution: :executed}, version: 1}
                        ]
                      }}

      assert_receive {:app_updated,
                      %{sessions: [%{app_status: %{execution: :executed}, version: 2}]}}
    end
  end

  describe "get_session_id/1" do
    test "starts a new session if none in single-session mode" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, auto_shutdown_ms: 5_000}
      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      assert %{sessions: []} = App.get_by_pid(app_pid)

      session_id = App.get_session_id(app_pid)
      assert ^session_id = App.get_session_id(app_pid)

      assert %{sessions: [%{id: ^session_id}]} = App.get_by_pid(app_pid)
    end

    test "returns an executed session if possible in single-session mode with zero-downtime deployment" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, zero_downtime: true}
      notebook = %{Notebook.new() | app_settings: app_settings}

      App.subscribe(slug)

      app_pid = start_app(notebook)

      assert_receive {:app_updated,
                      %{sessions: [%{id: session_id1, app_status: %{execution: :executed}}]}}

      App.deploy(app_pid, notebook)

      assert_receive {:app_updated,
                      %{
                        sessions: [
                          %{id: session_id2, app_status: %{execution: :executing}},
                          %{id: ^session_id1, app_status: %{execution: :executed}}
                        ]
                      }}

      assert ^session_id1 = App.get_session_id(app_pid)

      assert_receive {:app_updated,
                      %{sessions: [%{id: ^session_id2, app_status: %{execution: :executed}}]}}

      assert ^session_id2 = App.get_session_id(app_pid)
    end

    test "starts a new session in multi-session mode" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}
      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      assert %{sessions: []} = App.get_by_pid(app_pid)

      session_id1 = App.get_session_id(app_pid)
      session_id2 = App.get_session_id(app_pid)

      assert %{sessions: [%{id: ^session_id2}, %{id: ^session_id1}]} = App.get_by_pid(app_pid)
    end

    test "does not store session creator user with personal hub" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}
      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      assert %{sessions: []} = App.get_by_pid(app_pid)

      user = %{Livebook.Users.User.new() | name: "Jake Peralta"}
      session_id = App.get_session_id(app_pid, user: user)

      assert %{sessions: [%{id: ^session_id, started_by: nil}]} = App.get_by_pid(app_pid)
    end

    test "stores session creator user in multi-session mode with teams hub" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}
      notebook = %{Notebook.new() | app_settings: app_settings, teams_enabled: true}

      app_pid = start_app(notebook)

      assert %{sessions: []} = App.get_by_pid(app_pid)

      user = %{Livebook.Users.User.new() | name: "Jake Peralta"}
      session_id = App.get_session_id(app_pid, user: user)

      assert %{sessions: [%{id: ^session_id, started_by: ^user}]} = App.get_by_pid(app_pid)
    end
  end

  describe "automatic shutdown" do
    test "shuts down sessions on inactivity when configured to" do
      slug = Utils.random_short_id()

      app_settings = %{
        Notebook.AppSettings.new()
        | slug: slug,
          multi_session: true,
          auto_shutdown_ms: 1
      }

      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      App.subscribe(slug)

      session_id = App.get_session_id(app_pid)
      assert_receive {:app_updated, %{sessions: [%{id: ^session_id}]}}

      assert_receive {:app_updated, %{sessions: []}}
    end
  end

  defp start_app(notebook) do
    opts = [notebook: notebook]
    start_supervised!({App, opts})
  end
end
