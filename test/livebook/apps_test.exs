defmodule Livebook.AppsTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog

  describe "deploy_apps_in_dir/1" do
    @tag :tmp_dir
    test "deploys apps", %{tmp_dir: tmp_dir} do
      app1_path = Path.join(tmp_dir, "app1.livemd")
      app2_path = Path.join(tmp_dir, "app2.livemd")

      File.write!(app1_path, """
      <!-- livebook:{"app_settings":{"slug":"app1"}} -->

      # App 1
      """)

      File.write!(app2_path, """
      <!-- livebook:{"app_settings":{"slug":"app2"}} -->

      # App 2
      """)

      Livebook.Apps.subscribe()

      Livebook.Apps.deploy_apps_in_dir(tmp_dir)

      assert_receive {:app_created, %{slug: "app1"} = app1}

      assert_receive {:app_updated,
                      %{slug: "app1", sessions: [%{app_status: %{execution: :executed}}]}}

      assert_receive {:app_created, %{slug: "app2"} = app2}

      assert_receive {:app_updated,
                      %{slug: "app2", sessions: [%{app_status: %{execution: :executed}}]}}

      Livebook.App.close(app1.pid)
      Livebook.App.close(app2.pid)
    end

    @tag :tmp_dir
    test "skips apps with incomplete config and warns", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      File.write!(app_path, """
      # App
      """)

      assert capture_log(fn ->
               Livebook.Apps.deploy_apps_in_dir(tmp_dir)
             end) =~ "Skipping app deployment at #{app_path} due to invalid settings"
    end

    @tag :tmp_dir
    test "overrides apps password when :password is set", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"slug":"app"}} -->

      # App
      """)

      Livebook.Apps.subscribe()

      Livebook.Apps.deploy_apps_in_dir(tmp_dir, password: "verylongpass")

      assert_receive {:app_created, %{slug: "app"} = app}

      %{access_type: :protected, password: "verylongpass"} = Livebook.App.get_settings(app.pid)

      Livebook.App.close(app.pid)
    end
  end
end
