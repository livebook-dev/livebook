defmodule LivebookWeb.AppsLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.{Session, Sessions}

  test "updates UI when app is deployed and terminated", %{conn: conn} do
    session = start_session()

    Sessions.subscribe()

    slug = Livebook.Utils.random_short_id()
    app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
    Session.set_app_settings(session.pid, app_settings)

    Session.set_notebook_name(session.pid, "My app #{slug}")

    {:ok, view, _} = live(conn, ~p"/apps")

    refute render(view) =~ slug

    Session.deploy_app(session.pid)

    assert_receive {:session_created, %{app_info: %{slug: ^slug}}}
    assert render(view) =~ "My app #{slug}"

    assert_receive {:session_updated, %{app_info: %{slug: ^slug, registered: true}} = app_session}
    assert render(view) =~ ~p"/apps/#{slug}"

    Session.app_unregistered(app_session.pid)

    assert_receive {:session_closed, %{app_info: %{slug: ^slug}}}
    refute render(view) =~ slug
  end

  test "terminating an app", %{conn: conn} do
    session = start_session()

    Sessions.subscribe()

    slug = Livebook.Utils.random_short_id()
    app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
    Session.set_app_settings(session.pid, app_settings)

    {:ok, view, _} = live(conn, ~p"/apps")

    Session.deploy_app(session.pid)
    assert_receive {:session_created, %{app_info: %{slug: ^slug}}}

    assert_receive {:session_updated,
                    %{app_info: %{slug: ^slug, status: :running, registered: true}}}

    view
    |> element(~s/[data-app-slug="#{slug}"] button[aria-label="stop app"]/)
    |> render_click()

    assert_receive {:session_updated,
                    %{app_info: %{slug: ^slug, status: :stopped, registered: false}}}

    view
    |> element(~s/[data-app-slug="#{slug}"] button[aria-label="terminate app"]/)
    |> render_click()

    assert_receive {:session_closed, %{app_info: %{slug: ^slug}}}

    refute render(view) =~ slug
  end

  defp start_session() do
    {:ok, session} = Livebook.Sessions.create_session()
    on_exit(fn -> Session.close(session.pid) end)
    session
  end
end
