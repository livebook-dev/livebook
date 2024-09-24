defmodule Livebook.AppsTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog
  import Livebook.HubHelpers

  alias Livebook.Apps
  alias Livebook.Utils

  describe "build_app_specs_in_dir/1" do
    @tag :tmp_dir
    test "skips apps with incomplete config and warns", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      File.write!(app_path, """
      # App
      """)

      assert capture_log(fn ->
               assert [] = Apps.build_app_specs_in_dir(tmp_dir)
             end) =~
               "Ignoring app at app.livemd. The deployment settings are missing or invalid. Please configure them under the notebook deploy panel."
    end

    @tag :tmp_dir
    test "skips apps with a duplicate slug and warns", %{tmp_dir: tmp_dir} do
      app1_path = Path.join(tmp_dir, "app1.livemd")
      app2_path = Path.join(tmp_dir, "app2.livemd")

      slug = Utils.random_short_id()

      File.write!(app1_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"}} -->

      # App 1
      """)

      File.write!(app2_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"}} -->

      # App 2
      """)

      assert capture_log(fn ->
               assert [_] = Apps.build_app_specs_in_dir(tmp_dir)
             end) =~ "App with the same slug (#{slug}) is already present at"
    end

    @tag :tmp_dir
    test "warns when there are import warnings", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      slug = Utils.random_short_id()

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"}} -->

      # App

      ```elixir
      """)

      assert capture_log(fn ->
               assert [_] = Apps.build_app_specs_in_dir(tmp_dir)
             end) =~ "line 5 - fenced Code Block opened with"
    end

    @tag :tmp_dir
    test "warns when a notebook is protected and no :password is set", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      slug = Utils.random_short_id()

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"slug":"#{slug}"}} -->

      # App
      """)

      assert capture_log(fn ->
               assert [_] = Apps.build_app_specs_in_dir(tmp_dir)
             end) =~
               "The app at app.livemd will use a random password. You may want to set LIVEBOOK_APPS_PATH_PASSWORD or make the app public."
    end

    @tag :tmp_dir
    test "overrides apps password when :password is set", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      slug = Utils.random_short_id()

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"slug":"#{slug}"}} -->

      # App
      """)

      Apps.subscribe()

      assert [app_spec] = Apps.build_app_specs_in_dir(tmp_dir, password: "verylongpass")

      {:ok, %{notebook: notebook}} = Apps.AppSpec.load(app_spec, tmp_dir)

      assert %{access_type: :protected, password: "verylongpass"} = notebook.app_settings
    end

    @tag :tmp_dir
    test "imports apps with valid offline hub stamp", %{tmp_dir: tmp_dir} do
      app1_path = Path.join(tmp_dir, "app1.livemd")
      app2_path = Path.join(tmp_dir, "app2.livemd")
      app3_path = Path.join(tmp_dir, "app3.livemd")

      # Personal hub
      File.write!(app1_path, """
      <!-- livebook:{"app_settings":{"slug":"app1"}} -->

      # App
      """)

      # Offline hub with valid stamp
      File.write!(app2_path, """
      <!-- livebook:{"app_settings":{"slug":"app2"},"hub_id":"team-org-number-3079"} -->

      # App

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":134,"stamp":{"token":"XCP.0UjwZQFcT3wejYozhtkGIt6b-6Rvc42RBPkjVfrAnagq9wgDS2q7OhBwxLYncA","token_signature":"l2mD7sDTUOoaVgfjElpO5YyUcbeKO6udu9ReA5NI1ehV1cD90dXfo9EvuxbRK7D8t9qf8EEkUlKY17pNTdKIqJ5xD_Flxwq_Lh_qAfw7tmRJ--rujO8MpzRCC6kTIW9YB78KDQE-Yl-Hq49Rsp5aQQTNwmLvRdDVFIKjxI5s6Vb_npS8zzR_-YpgkalwxVPsobdpfLkIs4rxkQcNzeD3gifzjxF_izVrnWQmACcbNBIxZfmJmSdMvwnYQc4bTKjhPSZ25MHG-6W7CSR1G48x9DYdNcHTVqQoKvesxu2IQhVl8wd-7VzLf6T6OEEMGPozq4tYkMPKE4IRlI4XfQrDzA","version":1}} -->
      """)

      # Offline hub with invalid stamp
      File.write!(app3_path, """
      <!-- livebook:{"app_settings":{"slug":"app3"},"hub_id":"team-org-number-3079"} -->

      # App

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":111,"stamp":{"token":"invalid","token_signature":"invalid","version":1}} -->
      """)

      log =
        assert capture_log(fn ->
                 assert [%{slug: "app2"}] =
                          Apps.build_app_specs_in_dir(tmp_dir, hub_id: offline_hub().id)
               end)

      assert log =~
               "Ignoring app at app1.livemd. The notebook does not come from hub #{offline_hub().id}"

      assert log =~
               "Ignoring app at app3.livemd. The notebook does not have a valid stamp"
    end
  end

  describe "integration" do
    @tag :tmp_dir
    test "deploying apps with offline hub secrets", %{tmp_dir: tmp_dir} do
      app_path = Path.join(tmp_dir, "app.livemd")

      slug = "6kgwrh2d"

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"6kgwrh2d"},"hub_id":"team-org-number-3079"} -->

      # App

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":161,"stamp":{"token":"XCP.SHa_YbuqbAQXVSwGyrgskid5maz8KEdX1XCTGzGcaBGsieVkK50uw4QKfVLi1Wb9wwKM9yLxHxdxlsNOiKtn-kaqmBlTJWPUuYJWICFmIIVSsOi3r-g","token_signature":"UzYy7I-CIbxaZoKsV30ipdjhotJPOR4tCP6ZH5v6cnCe2F7kednP4aNUouTnoxUYwd4AZ59wGz1cCM7PYd8rE3IbECIbT4ixUpftI-hkW2OoymLRv5sjsfGeTPS8PvV9SXiZIG2320G3Kc1Spf4dToZfpNxAimD9xOpZLpRkI9MUH3nKG99yz1mZHuTgLNVS5yvHgAV_xRYtKwPnfwMLvQkD5Z9NacBPnqURVia90j1ueo7tEw8H1qH4VQU2Uh1XWIODIuTuLh55oe4MydGK5NgUhDMg2Zs9QaAN3ejoXHqWSub6k_VrHxIuke8T_xMIem6P25wdHGUZInSJX5x4Xw","version":1}} -->
      """)

      Apps.subscribe()

      hub = offline_hub()

      secret = %Livebook.Secrets.Secret{
        name: "DB_PASSWORD",
        value: "postgres",
        hub_id: hub.id
      }

      put_offline_hub_secret(secret)

      assert [app_spec] = Apps.build_app_specs_in_dir(tmp_dir)

      deployer_pid = Apps.Deployer.local_deployer()
      Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:app_created, %{pid: pid, slug: ^slug}}
      assert_receive {:app_updated, %{slug: ^slug, sessions: [session]}}

      assert %{secrets: %{"DB_PASSWORD" => ^secret}} = Livebook.Session.get_data(session.pid)

      remove_offline_hub_secret(secret)
      Livebook.App.close(pid)
    end
  end
end
