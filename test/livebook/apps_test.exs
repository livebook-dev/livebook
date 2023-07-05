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

    @tag :capture_log
    @tag :tmp_dir
    test "deploys apps with offline hub stamp", %{tmp_dir: tmp_dir} do
      hub =
        Livebook.Factory.build(:team,
          id: "team-org-number-2946",
          teams_key: "AleIxOFlwSiOS78WXtVU01ySmitjzy-5pAuCh4i1wZE",
          org_public_key:
            "MIIBCgKCAQEA2uRttEa6UvtiAUhv-MhPZvvlrCNeeL5n6oP4pliqoMBD7vsi4EvwnrqjCCicwHeT4y8Pu1kmzTelDAHEyO8alllBtfnZnQkPOqo1Y6c6qBHhcioc2FrNvdAydMiByhyn_aqNbFNeMMgy9ogHerAQ6XPrGSaXEvIcWn3myz-zxYdeEDW5G5W95o7Q0x7lokdVBUwXbazH0JVu_-1FUr7aOSjjuNHX6rXMRA3wr4n2SuhGOvihrX5IYRb733pae2aTOfJZGD_83eUPHTu_cPoUflcvIPtnVlGTxBgSX9Ayl1X3uDOnJsk2pxawFF6GxBMUKjMGyGDTg_lL45cgsWovXQIDAQAB",
          hub_name: "org-number-2946"
        )

      Livebook.Hubs.set_offline_hub(hub)

      app1_path = Path.join(tmp_dir, "app1.livemd")
      app2_path = Path.join(tmp_dir, "app2.livemd")
      app3_path = Path.join(tmp_dir, "app3.livemd")

      File.write!(app1_path, """
      <!-- livebook:{"app_settings":{"slug":"app1"}} -->

      # App 1
      """)

      File.write!(app2_path, """
      <!-- livebook:{"app_settings":{"slug":"app2"},"hub_id":"team-org-number-2946"} -->

      # App 2

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":111,"stamp":{"token":"QTEyOEdDTQ.yw3drh2WcwU8K6jS9Wp0HPupyX3qoc8iBmUXrMVKvSPnIOGEYMmu160e89E.xyzsr7PxSBrA8Elt.N3KyvcuTrFyMYpSl8WB1Sctv-1YjSjv_DCZoOVje_zXPYpm4iV_Ss5tVUSA7IWE.lV7grc6HYOYJrf0YYScPwQ","token_signature":"KSd-EhXw2CrmS9m4aZnPhTgWzlNdQNJ0wvYmuNvi8Pxaqb-prKO0FN_BTcPHtk4ZDHJaIFac-8dyefkCHpIElAc_N7vExgO9_7wSOJ8Hagip7DOxOBfqcR6iC17ejiw-2wWFJu0p6deaXpm2RWkWJU--wiU1cAHoKoJGqIsMMxNmgAkT44Pok0ni5BtnTfZjq_c2iPTYfP-8uU2WFIDmzEeOL-He5iWNUlixnf5Aj1YSVNldi6vTtR70xBRvlUxPCkWbt1x6XjanspY15j43PgVTo0EPM4kGCkS2HcWBZB_XscxZ4-V-WdpQ0pkv1goPdfDGDcAbjP7z8oum9_ZKNA","version":1}} -->
      """)

      File.write!(app3_path, """
      <!-- livebook:{"app_settings":{"slug":"app3"}} -->

      # App 2

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":111,"stamp":{"token":"invalid","token_signature":"invalid","version":1}} -->
      """)

      Livebook.Apps.subscribe()
      Livebook.Apps.deploy_apps_in_dir(tmp_dir)
      Livebook.Hubs.set_offline_hub(nil)

      refute_receive {:app_created, %{slug: "app1"}}
      assert_receive {:app_created, %{pid: pid, slug: "app2"}}
      refute_receive {:app_created, %{slug: "app3"}}

      assert_receive {:app_updated,
                      %{slug: "app2", sessions: [%{app_status: %{execution: :executed}}]}}

      Livebook.App.close(pid)
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
               "Skipping app deployment at #{app_path}. The deployment settings are missing or invalid. Please configure them under the notebook deploy panel."
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
  end
end
