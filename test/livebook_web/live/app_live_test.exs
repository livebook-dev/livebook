defmodule LivebookWeb.AppLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.Session

  test "render guidance when Kino output is empty", %{conn: conn} do
    session = start_session()

    Session.subscribe(session.id)

    slug = Livebook.Utils.random_short_id()
    app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
    Session.set_app_settings(session.pid, app_settings)

    Session.set_notebook_name(session.pid, "My app #{slug}")
    Session.deploy_app(session.pid)

    assert_receive {:operation, {:add_app, _, _, _}}
    assert_receive {:operation, {:set_app_registered, _, _, true}}

    {:ok, view, _} = live(conn, ~p"/apps/#{slug}")

    assert render(view) =~ """
           This deployed notebook is empty. Deployed apps only render Kino outputs.
                   Ensure you use Kino for interactive visualizations and dynamic content.\
           """
  end

  defp start_session() do
    {:ok, session} = Livebook.Sessions.create_session()
    on_exit(fn -> Session.close(session.pid) end)
    session
  end
end
