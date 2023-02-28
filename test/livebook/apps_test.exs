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

      Livebook.Sessions.subscribe()

      Livebook.Apps.deploy_apps_in_dir(tmp_dir)

      assert_receive {:session_created, %{app_info: %{slug: "app1"}}}

      assert_receive {:session_updated,
                      %{app_info: %{slug: "app1", status: :running, registered: true}} =
                        app1_session}

      assert_receive {:session_created, %{app_info: %{slug: "app2"}}}

      assert_receive {:session_updated,
                      %{app_info: %{slug: "app2", status: :running, registered: true}} =
                        app2_session}

      Livebook.Session.close([app1_session.pid, app2_session.pid])
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

      Livebook.Sessions.subscribe()

      Livebook.Apps.deploy_apps_in_dir(tmp_dir, password: "verylongpass")

      assert_receive {:session_created, %{app_info: %{slug: "app"}} = session}

      %{access_type: :protected, password: "verylongpass"} =
        Livebook.Session.get_app_settings(session.pid)
    end
  end
end
