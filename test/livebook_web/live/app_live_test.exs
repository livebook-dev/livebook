defmodule LivebookWeb.AppLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Livebook.SessionHelpers
  import Phoenix.LiveViewTest

  alias Livebook.Session

  test "renders only rich output when output type is rich", %{conn: conn} do
    session = start_session()

    Session.subscribe(session.id)

    slug = Livebook.Utils.random_short_id()
    app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug, output_type: :rich}
    Session.set_app_settings(session.pid, app_settings)

    section_id = insert_section(session.pid)
    insert_cell_with_output(session.pid, section_id, {:stdout, "Printed output"})
    insert_cell_with_output(session.pid, section_id, {:plain_text, "Custom text"})

    Session.deploy_app(session.pid)

    assert_receive {:operation, {:add_app, _, _, _}}
    assert_receive {:operation, {:set_app_registered, _, _, true}}

    {:ok, view, _} = live(conn, ~p"/apps/#{slug}")

    refute render(view) =~ "Printed output"
    assert render(view) =~ "Custom text"
  end

  defp start_session() do
    {:ok, session} = Livebook.Sessions.create_session()
    on_exit(fn -> Session.close(session.pid) end)
    session
  end
end
