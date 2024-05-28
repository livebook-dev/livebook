defmodule Livebook.Apps.ManagerTest do
  # Not async, because we alter global config (permanent apps) and we
  # also test restarting the global manager process
  use ExUnit.Case, async: false

  alias Livebook.Apps
  alias Livebook.Notebook

  setup do
    on_exit(fn ->
      Apps.set_startup_app_specs([])

      for app <- Apps.list_apps() do
        Livebook.App.close(app.pid)
      end
    end)
  end

  @tag :tmp_dir
  test "restarts permanent apps on crash", %{tmp_dir: tmp_dir} do
    slug = Livebook.Utils.random_short_id()
    app_spec = path_app_spec(tmp_dir, slug)

    Apps.subscribe()

    Apps.set_startup_app_specs([app_spec])
    Apps.Manager.sync_permanent_apps()

    assert_receive {:app_created, %{slug: ^slug} = app}
    # Make sure the app finished init
    _ = Livebook.App.get_by_pid(app.pid)

    Process.exit(app.pid, :kill)
    assert_receive {:app_closed, %{slug: ^slug}}

    # Automatically restarted by the manager
    assert_receive {:app_created, %{slug: ^slug}}
  end

  @tag :tmp_dir
  test "closes no-longer-permanent apps", %{tmp_dir: tmp_dir} do
    slug1 = Livebook.Utils.random_short_id()
    app_spec1 = path_app_spec(tmp_dir, slug1)
    slug2 = Livebook.Utils.random_short_id()
    app_spec2 = path_app_spec(tmp_dir, slug2)

    Apps.subscribe()

    Apps.set_startup_app_specs([app_spec1])
    Apps.Manager.sync_permanent_apps()

    assert_receive {:app_created, %{slug: ^slug1}}

    Apps.set_startup_app_specs([app_spec2])
    Apps.Manager.sync_permanent_apps()

    assert_receive {:app_created, %{slug: ^slug2}}
    assert_receive {:app_closed, %{slug: ^slug1}}
  end

  @tag :tmp_dir
  test "redeploys an app when the spec version changes", %{tmp_dir: tmp_dir} do
    slug = Livebook.Utils.random_short_id()
    app_spec = path_app_spec(tmp_dir, slug)

    Apps.subscribe()

    Apps.set_startup_app_specs([app_spec])
    Apps.Manager.sync_permanent_apps()

    assert_receive {:app_created, %{slug: ^slug, version: 1}}

    app_spec = %{app_spec | version: "2"}
    Apps.set_startup_app_specs([app_spec])
    Apps.Manager.sync_permanent_apps()

    # Automatically redeployed by the manager
    assert_receive {:app_updated, %{slug: ^slug, version: 2, app_spec: ^app_spec}}
  end

  test "retries deployment on failure" do
    slug = Livebook.Utils.random_short_id()
    app_settings = %{Notebook.AppSettings.new() | slug: slug}
    notebook = %{Notebook.new() | app_settings: app_settings}
    app_spec = Apps.NotebookAppSpec.new(notebook, load_failures: 1)

    Apps.subscribe()

    assert ExUnit.CaptureLog.capture_log(fn ->
             Apps.set_startup_app_specs([app_spec])
             Apps.Manager.sync_permanent_apps()

             assert_receive {:app_created, %{slug: ^slug}}
           end) =~ "[app=#{slug}] Deployment failed, failed to load"
  end

  test "is restarted on crash" do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "manager_watcher")

    pid = :global.whereis_name(Apps.Manager)
    is_pid(pid) && Process.exit(pid, :kill)

    assert_receive {:manager_started, pid}
    assert :global.whereis_name(Apps.Manager) == pid
  end

  test "sends status events about running app specs" do
    slug = Livebook.Utils.random_short_id()
    app_settings = %{Notebook.AppSettings.new() | slug: slug}
    notebook = %{Notebook.new() | app_settings: app_settings}
    app_spec = Apps.NotebookAppSpec.new(notebook)

    Apps.Manager.subscribe()

    Apps.set_startup_app_specs([app_spec])
    Apps.Manager.sync_permanent_apps()

    assert_receive {:apps_manager_status, [%{app_spec: ^app_spec, running?: false}]}
    assert_receive {:apps_manager_status, [%{app_spec: ^app_spec, running?: true}]}

    # Version change
    app_spec_v2 = %{app_spec | version: "2"}
    Apps.set_startup_app_specs([app_spec_v2])
    Apps.Manager.sync_permanent_apps()

    assert_receive {:apps_manager_status,
                    [
                      %{app_spec: ^app_spec, running?: true},
                      %{app_spec: ^app_spec_v2, running?: false}
                    ]}

    assert_receive {:apps_manager_status, [%{app_spec: ^app_spec_v2, running?: true}]}

    # Restart
    {:ok, app} = Apps.fetch_app(app_spec.slug)
    Process.exit(app.pid, :kill)

    assert_receive {:apps_manager_status, [%{app_spec: ^app_spec_v2, running?: false}]}
    assert_receive {:apps_manager_status, [%{app_spec: ^app_spec_v2, running?: true}]}
  end

  defp path_app_spec(tmp_dir, slug) do
    app_path = Path.join(tmp_dir, "app_#{slug}.livemd")

    File.write!(app_path, """
    <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"}} -->

    # App
    """)

    %Livebook.Apps.PathAppSpec{slug: slug, path: app_path}
  end
end
