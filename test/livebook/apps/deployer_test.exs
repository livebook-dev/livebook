defmodule Livebook.Apps.DeployerTest do
  use ExUnit.Case, async: true

  alias Livebook.Apps
  alias Livebook.Notebook

  describe "deploy_monitor/3" do
    test "deploys and registers an app" do
      slug = Livebook.Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug}
      notebook = %{Notebook.new() | app_settings: app_settings}
      app_spec = Apps.NotebookAppSpec.new(notebook)

      Apps.subscribe()

      deployer_pid = Apps.Deployer.local_deployer()
      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:deploy_result, ^ref, {:ok, pid}}
      assert_receive {:app_created, %{pid: ^pid}}

      assert {:ok, %{pid: ^pid}} = Apps.fetch_app(slug)

      Livebook.App.close(pid)
    end

    @tag :tmp_dir
    test "deploys an app with files", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      files_path = Path.join(tmp_dir, "files")
      File.mkdir_p!(files_path)
      files_path |> Path.join("image.jpg") |> File.write!("content")

      slug = Livebook.Utils.random_short_id()

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"file_entries":[{"name":"image.jpg","type":"attachment"}]} -->

      # App
      """)

      app_spec = %Livebook.Apps.PathAppSpec{slug: slug, path: app_path}

      Apps.subscribe()

      deployer_pid = Apps.Deployer.local_deployer()
      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:deploy_result, ^ref, {:ok, pid}}
      assert_receive {:app_created, %{pid: ^pid}}

      assert_receive {:app_updated, %{pid: ^pid, sessions: [%{pid: session_pid}]}}

      session = Livebook.Session.get_by_pid(session_pid)

      assert Livebook.FileSystem.File.resolve(session.files_dir, "image.jpg")
             |> Livebook.FileSystem.File.read() == {:ok, "content"}

      Livebook.App.close(pid)
    end

    test "deploys a new app version if already running" do
      slug = Livebook.Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug}
      notebook = %{Notebook.new() | app_settings: app_settings}
      app_spec = Apps.NotebookAppSpec.new(notebook)

      Apps.subscribe()

      deployer_pid = Apps.Deployer.local_deployer()
      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:deploy_result, ^ref, {:ok, pid}}
      assert_receive {:app_created, %{pid: ^pid, version: 1}}

      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:deploy_result, ^ref, {:ok, ^pid}}
      assert_receive {:app_updated, %{pid: ^pid, version: 2}}

      Livebook.App.close(pid)
    end

    test "sends an error when the deployment fails" do
      slug = Livebook.Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug}
      notebook = %{Notebook.new() | app_settings: app_settings}

      app_spec = Apps.NotebookAppSpec.new(notebook, load_failures: 1)

      deployer_pid = Apps.Deployer.local_deployer()
      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:deploy_result, ^ref, {:error, "failed to load"}}
    end

    test "does not redeploy when :start_only is specified" do
      slug = Livebook.Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug}
      notebook = %{Notebook.new() | app_settings: app_settings}
      app_spec = Apps.NotebookAppSpec.new(notebook)

      Apps.subscribe()

      deployer_pid = Apps.Deployer.local_deployer()
      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:deploy_result, ^ref, {:ok, pid}}
      assert_receive {:app_created, %{pid: ^pid, version: 1}}

      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec, start_only: true)

      assert_receive {:deploy_result, ^ref, {:error, :already_started}}

      Livebook.App.close(pid)
    end

    @tag :tmp_dir
    test "runs warmup if enabled by app spec", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "setup.txt")

      slug = Livebook.Utils.random_short_id()
      # Multi-session, so that no session is started eagerly
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}

      notebook =
        %{Notebook.new() | app_settings: app_settings}
        |> Notebook.put_setup_cells([
          %{
            Notebook.Cell.new(:code)
            | source: """
              File.touch!("#{path}")
              """
          }
        ])

      app_spec = Apps.NotebookAppSpec.new(notebook, should_warmup: true)

      deployer_pid = Apps.Deployer.local_deployer()
      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:deploy_result, ^ref, {:ok, pid}}

      assert File.exists?(path)

      Livebook.App.close(pid)
    end

    test "warns if the warmup fails" do
      slug = Livebook.Utils.random_short_id()
      # Multi-session, so that no session is started eagerly
      app_settings = %{Notebook.AppSettings.new() | slug: slug, multi_session: true}

      notebook =
        %{Notebook.new() | app_settings: app_settings}
        |> Notebook.put_setup_cells([
          %{
            Notebook.Cell.new(:code)
            | source: """
              raise "error"
              """
          }
        ])

      app_spec = Apps.NotebookAppSpec.new(notebook, should_warmup: true)

      deployer_pid = Apps.Deployer.local_deployer()

      assert ExUnit.CaptureLog.capture_log(fn ->
               ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

               assert_receive {:deploy_result, ^ref, {:ok, pid}}

               Livebook.App.close(pid)
             end) =~ "[app=#{slug}] App warmup failed, setup cell finished with failure"
    end

    test "sends monitor message when deployer crashes" do
      slug = Livebook.Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug}

      notebook =
        %{Notebook.new() | app_settings: app_settings}
        |> Notebook.put_setup_cells([
          %{
            Notebook.Cell.new(:code)
            | source: """
              Process.sleep(:infinity)
              """
          }
        ])

      app_spec = Apps.NotebookAppSpec.new(notebook, should_warmup: true)

      deployer_pid = start_supervised!(Apps.Deployer)

      ref = Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      # The deployer is stuck at warmup and we kill it
      Process.exit(deployer_pid, :kill)

      assert_receive {:DOWN, ^ref, :process, _pid, _reason}
    end
  end
end
