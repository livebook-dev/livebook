defmodule LivebookWeb.AppsDashboardLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers
  import Livebook.AppHelpers

  alias Livebook.App
  alias Livebook.Apps
  alias Livebook.Notebook
  alias Livebook.Utils

  test "updates UI when app is deployed and terminated", %{conn: conn} do
    slug = Utils.random_short_id()
    app_settings = %{Notebook.AppSettings.new() | slug: slug}
    notebook = %{Notebook.new() | app_settings: app_settings, name: "My app #{slug}"}

    {:ok, view, _} = live(conn, ~p"/apps-dashboard")

    refute render(view) =~ slug

    Apps.subscribe()
    app_pid = deploy_notebook_sync(notebook)

    assert_receive {:app_created, %{pid: ^app_pid}}
    assert render(view) =~ "My app #{slug}"

    App.close(app_pid)

    assert_receive {:app_closed, %{pid: ^app_pid}}
    refute render(view) =~ slug
  end

  test "terminating an app", %{conn: conn} do
    slug = Utils.random_short_id()
    app_settings = %{Notebook.AppSettings.new() | slug: slug}
    notebook = %{Notebook.new() | app_settings: app_settings, name: "My app #{slug}"}

    {:ok, view, _} = live(conn, ~p"/apps-dashboard")

    Apps.subscribe()
    app_pid = deploy_notebook_sync(notebook)

    assert_receive {:app_created, %{pid: ^app_pid}}

    view
    |> element(~s/[data-app-slug="#{slug}"] button[aria-label="terminate app"]/)
    |> render_click()

    render_confirm(view)

    assert_receive {:app_closed, %{pid: ^app_pid}}
  end

  test "deactivating and terminating an app session", %{conn: conn} do
    slug = Utils.random_short_id()
    app_settings = %{Notebook.AppSettings.new() | slug: slug}
    notebook = %{Notebook.new() | app_settings: app_settings, name: "My app #{slug}"}

    {:ok, view, _} = live(conn, ~p"/apps-dashboard")

    Apps.subscribe()
    app_pid = deploy_notebook_sync(notebook)

    assert_receive {:app_created, %{pid: ^app_pid}}

    assert_receive {:app_updated,
                    %{pid: ^app_pid, sessions: [%{app_status: %{execution: :executed}}]}}

    view
    |> element(~s/[data-app-slug="#{slug}"] button[aria-label="deactivate app session"]/)
    |> render_click()

    assert_receive {:app_updated,
                    %{pid: ^app_pid, sessions: [%{app_status: %{lifecycle: :deactivated}}]}}

    view
    |> element(~s/[data-app-slug="#{slug}"] button[aria-label="terminate app session"]/)
    |> render_click()

    assert_receive {:app_updated, %{pid: ^app_pid, sessions: []}}

    App.close(app_pid)
  end
end
