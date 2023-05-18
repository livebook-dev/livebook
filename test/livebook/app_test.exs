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

    test "does not eagerly start session given auto shutdown on inactivity" do
      slug = Utils.random_short_id()

      app_settings = %{
        Notebook.AppSettings.new()
        | slug: slug,
          auto_shutdown_type: :inactive_5s
      }

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

    test "keeps old sessions when auto shutdown is set to never" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, auto_shutdown_type: :never}
      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      App.subscribe(slug)

      App.deploy(app_pid, notebook)

      assert_receive {:app_updated,
                      %{
                        sessions: [
                          %{app_status: :executed, version: 2},
                          %{app_status: :executed, version: 1}
                        ]
                      }}
    end

    test "shuts down old sessions when auto shutdown is set to new version" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, auto_shutdown_type: :new_version}
      notebook = %{Notebook.new() | app_settings: app_settings}

      App.subscribe(slug)

      app_pid = start_app(notebook)
      assert_receive {:app_updated, %{sessions: [%{app_status: :executed, version: 1}]}}

      App.deploy(app_pid, notebook)

      assert_receive {:app_updated, %{sessions: [%{app_status: :executing, version: 2}]}}
      assert_receive {:app_updated, %{sessions: [%{app_status: :executed, version: 2}]}}
    end

    test "keeps old executed session during single-session zero-downtime deployment" do
      slug = Utils.random_short_id()

      app_settings = %{
        Notebook.AppSettings.new()
        | slug: slug,
          auto_shutdown_type: :new_version,
          zero_downtime: true
      }

      notebook = %{Notebook.new() | app_settings: app_settings}

      App.subscribe(slug)

      app_pid = start_app(notebook)
      assert_receive {:app_updated, %{sessions: [%{app_status: :executed, version: 1}]}}

      App.deploy(app_pid, notebook)

      assert_receive {:app_updated,
                      %{
                        sessions: [
                          %{app_status: :executing, version: 2},
                          %{app_status: :executed, version: 1}
                        ]
                      }}

      assert_receive {:app_updated, %{sessions: [%{app_status: :executed, version: 2}]}}
    end
  end

  describe "get_session_id/1" do
    test "starts a new session if none in single-session mode" do
      slug = Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, auto_shutdown_type: :inactive_5s}
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
      assert_receive {:app_updated, %{sessions: [%{id: session_id1, app_status: :executed}]}}

      App.deploy(app_pid, notebook)

      assert_receive {:app_updated,
                      %{
                        sessions: [
                          %{id: session_id2, app_status: :executing},
                          %{id: ^session_id1, app_status: :executed}
                        ]
                      }}

      assert ^session_id1 = App.get_session_id(app_pid)

      assert_receive {:app_updated, %{sessions: [%{id: ^session_id2, app_status: :executed}]}}
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
  end

  describe "automatic shutdown" do
    test "shuts down sessions on inactivity when configured to" do
      slug = Utils.random_short_id()

      app_settings = %{
        Notebook.AppSettings.new()
        | slug: slug,
          multi_session: true,
          auto_shutdown_type: :inactive_5s
      }

      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      App.subscribe(slug)

      session_id = App.get_session_id(app_pid)
      assert_receive {:app_updated, %{sessions: [%{id: ^session_id}]}}

      # Should close in 50ms
      assert_receive {:app_updated, %{sessions: []}}, 70
    end

    test "does not count inactivity while there are connected clients" do
      slug = Utils.random_short_id()

      app_settings = %{
        Notebook.AppSettings.new()
        | slug: slug,
          multi_session: true,
          auto_shutdown_type: :inactive_5s
      }

      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      App.subscribe(slug)

      session_id = App.get_session_id(app_pid)
      assert_receive {:app_updated, %{sessions: [%{id: ^session_id, pid: session_pid}]}}

      client_pid = spawn_link(fn -> receive do: (:stop -> :ok) end)
      user = Livebook.Users.User.new()
      {_, _client_id} = Livebook.Session.register_client(session_pid, client_pid, user)

      refute_receive {:app_updated, %{sessions: []}}, 70
      send(client_pid, :stop)
      assert_receive {:app_updated, %{sessions: []}}, 70
    end

    test "reschedules inactivity timers when a new auto shutdown type is deployed" do
      slug = Utils.random_short_id()

      app_settings = %{
        Notebook.AppSettings.new()
        | slug: slug,
          multi_session: true,
          auto_shutdown_type: :never
      }

      notebook = %{Notebook.new() | app_settings: app_settings}

      app_pid = start_app(notebook)

      App.subscribe(slug)

      session_id = App.get_session_id(app_pid)
      assert_receive {:app_updated, %{sessions: [%{id: ^session_id}]}}

      Process.sleep(50)

      notebook = put_in(notebook.app_settings.auto_shutdown_type, :inactive_5s)
      App.deploy(app_pid, notebook)

      # It's been 50ms of inactivity, so the session should be closed right away
      assert_receive {:app_updated, %{sessions: []}}, 10
    end
  end

  defp start_app(notebook) do
    auto_shutdown_inactivity_ms = %{inactive_5s: 50, inactive_1m: 70_000, inactive_1h: 3700_000}
    opts = [notebook: notebook, auto_shutdown_inactivity_ms: auto_shutdown_inactivity_ms]
    start_supervised!({App, opts})
  end
end
