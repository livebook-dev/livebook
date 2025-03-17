defmodule LivebookWeb.Integration.AdminLiveTest do
  # Not async, because we alter global config (app server instance)
  use Livebook.TeamsIntegrationCase, async: false

  import Phoenix.LiveViewTest

  setup %{teams_auth: teams_auth} do
    Application.put_env(:livebook, :teams_auth, teams_auth)
    on_exit(fn -> Application.delete_env(:livebook, :teams_auth) end)

    :ok
  end

  for page <- ["/", "/settings", "/learn", "/hub", "/apps-dashboard"] do
    @tag page: page, teams_auth: :online
    test "GET #{page} shows the app server instance topbar warning", %{conn: conn, page: page} do
      {:ok, view, _} = live(conn, page)

      assert render(view) =~
               "This Livebook instance has been configured for notebook deployment and is in read-only mode."
    end

    @tag page: page, teams_auth: :offline
    test "GET #{page} shows the offline hub topbar warning", %{conn: conn, page: page} do
      {:ok, view, _} = live(conn, page)

      assert render(view) =~
               "You are running an offline Workspace for deployment. You cannot modify its settings."
    end
  end
end
