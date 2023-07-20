defmodule Livebook.AppsTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog
  import Livebook.HubHelpers

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

    @tag :capture_log
    @tag :tmp_dir
    test "deploys apps with offline hub stamp", %{tmp_dir: tmp_dir} do
      app1_path = Path.join(tmp_dir, "app1.livemd")
      app2_path = Path.join(tmp_dir, "app2.livemd")
      app3_path = Path.join(tmp_dir, "app3.livemd")

      File.write!(app1_path, """
      <!-- livebook:{"app_settings":{"slug":"offline_hub_app1"}} -->

      # App 1
      """)

      File.write!(app2_path, """
      <!-- livebook:{"app_settings":{"slug":"offline_hub_app2"},"hub_id":"team-org-number-3079"} -->

      # App 2

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":148,"stamp":{"token":"QTEyOEdDTQ.M486X5ISDg_wVwvndrMYJKIkfXa5qAwggh5LzkV41wVOY8SNQvfA4EFx4Tk.RrcteIrzJVrqQdhH.mtmu5KFj.bibmp2rxZoniYX1xY8dTvw","token_signature":"28ucTCoDXxahOIMg7SvdYIoLpGIUSahEa7mchH0jKncKeZH8-w-vOaL1F1uj_94lqQJFkmHWv988__1bPmYCorw7F1wAvAaprt3o2vSitWWmBszuF5JaimkFqOFcK3mc4NHuswQKuBjSL0W_yR-viiwlx6zPNsTpftVKjRI2Cri1PsMeZgahfdR2gy1OEgavzU6J6YWsNQHIMWgt5gwT6fIua1zaV7K8-TA6-6NRgcfG-pSJqRIm-3-vnbH5lkXRCgXCo_S9zWa6Jrcl5AbLObSr5DUueiwac1RobH7jNghCm1F-o1cUk9W-BJRZ7igVMwaYqLaOnKO8ya9CrkIiMg","version":1}} -->
      """)

      File.write!(app3_path, """
      <!-- livebook:{"app_settings":{"slug":"offline_hub_app3"}} -->

      # App 3

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":111,"stamp":{"token":"invalid","token_signature":"invalid","version":1}} -->
      """)

      Livebook.Apps.subscribe()

      Application.put_env(:livebook, :apps_path_hub_id, offline_hub().id, persistent: true)
      Livebook.Apps.deploy_apps_in_dir(tmp_dir)
      Application.delete_env(:livebook, :apps_path_hub_id, persistent: true)

      refute_receive {:app_created, %{slug: "offline_hub_app1"}}
      assert_receive {:app_created, %{pid: pid, slug: "offline_hub_app2"}}
      refute_receive {:app_created, %{slug: "offline_hub_app3"}}

      assert_receive {:app_updated,
                      %{
                        slug: "offline_hub_app2",
                        sessions: [%{app_status: %{execution: :executed}}]
                      }}

      Livebook.App.close(pid)
    end

    @tag :tmp_dir
    test "deploys apps with offline hub secrets", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app1.livemd")
      hub = offline_hub()

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"slug":"offline_hub_app2"},"hub_id":"team-org-number-3079"} -->

      # App 2

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":148,"stamp":{"token":"QTEyOEdDTQ.M486X5ISDg_wVwvndrMYJKIkfXa5qAwggh5LzkV41wVOY8SNQvfA4EFx4Tk.RrcteIrzJVrqQdhH.mtmu5KFj.bibmp2rxZoniYX1xY8dTvw","token_signature":"28ucTCoDXxahOIMg7SvdYIoLpGIUSahEa7mchH0jKncKeZH8-w-vOaL1F1uj_94lqQJFkmHWv988__1bPmYCorw7F1wAvAaprt3o2vSitWWmBszuF5JaimkFqOFcK3mc4NHuswQKuBjSL0W_yR-viiwlx6zPNsTpftVKjRI2Cri1PsMeZgahfdR2gy1OEgavzU6J6YWsNQHIMWgt5gwT6fIua1zaV7K8-TA6-6NRgcfG-pSJqRIm-3-vnbH5lkXRCgXCo_S9zWa6Jrcl5AbLObSr5DUueiwac1RobH7jNghCm1F-o1cUk9W-BJRZ7igVMwaYqLaOnKO8ya9CrkIiMg","version":1}} -->
      """)

      Livebook.Apps.subscribe()

      secret = %Livebook.Secrets.Secret{
        name: "DB_PASSWORD",
        value: "postgres",
        hub_id: hub.id
      }

      put_offline_hub_secret(secret)
      assert secret in Livebook.Hubs.get_secrets(hub)

      Livebook.Apps.deploy_apps_in_dir(tmp_dir)

      assert_receive {:app_created, %{pid: pid, slug: "offline_hub_app2"}}

      assert_receive {:app_updated,
                      %{
                        slug: "offline_hub_app2",
                        sessions: [%{app_status: %{execution: :executed}}]
                      }}

      session_id = Livebook.App.get_session_id(pid)
      {:ok, session} = Livebook.Sessions.fetch_session(session_id)
      assert %{hub_secrets: [^secret]} = Livebook.Session.get_data(session.pid)

      Livebook.App.close(pid)
      remove_offline_hub_secret(secret)
    end

    @tag :tmp_dir
    test "skips apps with incomplete config and warns", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      File.write!(app_path, """
      # App
      """)

      assert capture_log(fn ->
               Livebook.Apps.deploy_apps_in_dir(tmp_dir)
             end) =~
               "Skipping deployment for app at app.livemd. The deployment settings are missing or invalid. Please configure them under the notebook deploy panel."
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

    @tag :capture_log
    @tag :tmp_dir
    test "deploys with import warnings", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"slug":"app"}} -->

      # App

      ```elixir
      """)

      Livebook.Apps.subscribe()

      Livebook.Apps.deploy_apps_in_dir(tmp_dir)

      assert_receive {:app_created, %{slug: "app", warnings: warnings} = app}

      assert warnings == [
               "Import: line 5 - fenced Code Block opened with ``` not closed at end of input"
             ]

      Livebook.App.close(app.pid)
    end

    @tag :tmp_dir
    test "deploys notebook with attachment files", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")
      files_path = Path.join(tmp_dir, "files")
      File.mkdir_p!(files_path)
      image_path = Path.join(files_path, "image.jpg")
      File.write!(image_path, "content")

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"slug":"app"},"file_entries":[{"name":"image.jpg","type":"attachment"}]} -->

      # App
      """)

      Livebook.Apps.subscribe()

      Livebook.Apps.deploy_apps_in_dir(tmp_dir)

      assert_receive {:app_created, %{slug: "app"} = app}

      session_id = Livebook.App.get_session_id(app.pid)
      {:ok, session} = Livebook.Sessions.fetch_session(session_id)

      assert Livebook.FileSystem.File.resolve(session.files_dir, "image.jpg")
             |> Livebook.FileSystem.File.read() == {:ok, "content"}

      Livebook.App.close(app.pid)
    end
  end
end
