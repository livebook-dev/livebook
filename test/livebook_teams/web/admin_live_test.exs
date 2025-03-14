defmodule LivebookWeb.Integration.AdminLiveTest do
  # Not async, because we alter global config (app server instance)
  use Livebook.TeamsIntegrationCase, async: false

  import Phoenix.LiveViewTest

  setup do
    Application.put_env(:livebook, :teams_auth?, true)
    on_exit(fn -> Application.put_env(:livebook, :teams_auth?, false) end)

    :ok
  end

  for page <- ["/", "/settings", "/learn", "/hub", "/apps-dashboard"] do
    @tag page: page
    test "GET #{page} shows the app server topbar warning", %{conn: conn, page: page} do
      {:ok, view, _} = live(conn, page)

      assert render(view) =~
               "This Livebook instance has been configured for notebook deployment and is in read-only mode."
    end
  end
end
