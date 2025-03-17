defmodule LivebookWeb.Integration.AdminLiveTest do
  # Not async, because we alter global config (app server instance)
  use Livebook.TeamsIntegrationCase, async: false

  import Phoenix.LiveViewTest

  describe "app server" do
    setup do
      Application.put_env(
        :livebook,
        :app_server_instance_warning,
        "This Livebook instance has been configured for notebook deployment and is in read-only mode."
      )

      on_exit(fn -> Application.delete_env(:livebook, :app_server_instance_warning) end)

      :ok
    end

    for page <- ["/", "/settings", "/learn", "/hub", "/apps-dashboard"] do
      @tag page: page
      test "GET #{page} shows the app server instance topbar warning", %{conn: conn, page: page} do
        {:ok, view, _} = live(conn, page)

        assert render(view) =~
                 "This Livebook instance has been configured for notebook deployment and is in read-only mode."
      end
    end
  end

  describe "offline hub" do
    setup do
      Application.put_env(
        :livebook,
        :app_server_instance_warning,
        "You are running an offline Workspace for deployment. You cannot modify its settings."
      )

      on_exit(fn -> Application.delete_env(:livebook, :app_server_instance_warning) end)

      :ok
    end

    for page <- ["/", "/settings", "/learn", "/hub", "/apps-dashboard"] do
      @tag page: page
      test "GET #{page} shows the offline hub topbar warning", %{conn: conn, page: page} do
        {:ok, view, _} = live(conn, page)

        assert render(view) =~
                 "You are running an offline Workspace for deployment. You cannot modify its settings."
      end
    end
  end
end
