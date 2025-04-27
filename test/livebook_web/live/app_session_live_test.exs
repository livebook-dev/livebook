defmodule LivebookWeb.AppSessionLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers
  import Livebook.AppHelpers

  alias Livebook.App
  alias Livebook.Apps
  alias Livebook.Notebook
  alias Livebook.Utils

  test "shows a nonexisting message if the session does not exist", %{conn: conn} do
    slug = Utils.random_short_id()
    app_settings = %{Notebook.AppSettings.new() | slug: slug}
    notebook = %{Notebook.new() | app_settings: app_settings}

    app_pid = deploy_notebook_sync(notebook)

    {:ok, view, _} = live(conn, ~p"/apps/#{slug}/sessions/nonexistent")
    assert render(view) =~ "This app session does not exist"
    assert render(view) =~ ~p"/apps/#{slug}"

    App.close(app_pid)
  end

  test "shows a nonexisting message if the session is deactivated", %{conn: conn} do
    slug = Utils.random_short_id()
    app_settings = %{Notebook.AppSettings.new() | slug: slug}
    notebook = %{Notebook.new() | app_settings: app_settings}

    Apps.subscribe()
    app_pid = deploy_notebook_sync(notebook)

    assert_receive {:app_created, %{pid: ^app_pid}}

    assert_receive {:app_updated,
                    %{pid: ^app_pid, sessions: [%{id: session_id, pid: session_pid}]}}

    Livebook.Session.app_deactivate(session_pid)

    assert_receive {:app_updated,
                    %{pid: ^app_pid, sessions: [%{app_status: %{lifecycle: :deactivated}}]}}

    {:ok, view, _} = live(conn, ~p"/apps/#{slug}/sessions/#{session_id}")
    assert render(view) =~ "This app session does not exist"
    assert render(view) =~ ~p"/apps/#{slug}"

    App.close(app_pid)
  end

  test "redirects to homepage if the session gets deactivated", %{conn: conn} do
    slug = Utils.random_short_id()
    app_settings = %{Notebook.AppSettings.new() | slug: slug}
    notebook = %{Notebook.new() | app_settings: app_settings}

    Apps.subscribe()
    app_pid = deploy_notebook_sync(notebook)

    assert_receive {:app_created, %{pid: ^app_pid}}

    assert_receive {:app_updated,
                    %{pid: ^app_pid, sessions: [%{id: session_id, pid: session_pid}]}}

    {:ok, view, _} = live(conn, ~p"/apps/#{slug}/sessions/#{session_id}")

    Livebook.Session.app_deactivate(session_pid)

    flash = assert_redirect(view, ~p"/")
    assert flash["info"] == "Session has been closed"

    App.close(app_pid)
  end

  test "renders only rich output when output type is rich", %{conn: conn} do
    slug = Livebook.Utils.random_short_id()
    app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug, output_type: :rich}

    notebook = %{
      Livebook.Notebook.new()
      | app_settings: app_settings,
        sections: [
          %{
            Livebook.Notebook.Section.new()
            | cells: [
                %{
                  Livebook.Notebook.Cell.new(:code)
                  | source:
                      source_for_output(%{
                        type: :terminal_text,
                        text: "Printed output",
                        chunk: false
                      })
                },
                %{
                  Livebook.Notebook.Cell.new(:code)
                  | source:
                      source_for_output(%{type: :plain_text, text: "Custom text", chunk: false})
                }
              ]
          }
        ]
    }

    Livebook.Apps.subscribe()
    app_pid = deploy_notebook_sync(notebook)

    assert_receive {:app_created, %{pid: ^app_pid} = app}

    assert_receive {:app_updated,
                    %{pid: ^app_pid, sessions: [%{app_status: %{execution: :executed}}]}}

    {:ok, view, _} = conn |> live(~p"/apps/#{slug}") |> follow_redirect(conn)

    refute render(view) =~ "Printed output"
    assert render(view) =~ "Custom text"

    Livebook.App.close(app.pid)
  end

  test "shows an error message when session errors", %{conn: conn} do
    slug = Livebook.Utils.random_short_id()
    app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}

    id = Livebook.Utils.random_short_id() |> String.to_atom()

    notebook = %{
      Livebook.Notebook.new()
      | app_settings: app_settings,
        sections: [
          %{
            Livebook.Notebook.Section.new()
            | cells: [
                %{
                  Livebook.Notebook.Cell.new(:code)
                  | source:
                      source_for_output(%{
                        type: :terminal_text,
                        text: "Printed output",
                        chunk: false
                      })
                },
                %{
                  Livebook.Notebook.Cell.new(:code)
                  | id: "error-cell",
                    source: """
                    # Fail on the first run
                    unless :persistent_term.get(#{inspect(id)}, false) do
                      :persistent_term.put(#{inspect(id)}, true)
                      raise "oops"
                    end
                    """
                }
              ]
          }
        ]
    }

    Livebook.Apps.subscribe()
    app_pid = deploy_notebook_sync(notebook)

    assert_receive {:app_created, %{pid: ^app_pid} = app}

    assert_receive {:app_updated,
                    %{
                      pid: ^app_pid,
                      sessions: [%{id: session_id, app_status: %{execution: :error}}]
                    }}

    {:ok, view, _} = conn |> live(~p"/apps/#{slug}") |> follow_redirect(conn)

    assert render(view) =~ "Printed output"
    assert render(view) =~ "Something went wrong"
    assert render(view) =~ ~p"/sessions/#{session_id}" <> "#cell-error-cell"

    view
    |> element("button", "Retry")
    |> render_click()

    assert_receive {:app_updated,
                    %{pid: ^app_pid, sessions: [%{app_status: %{execution: :executed}}]}}

    Livebook.App.close(app.pid)
  end

  test "shows the reprocessing button when there are changed inputs and no errors",
       %{conn: conn, test: test} do
    slug = Livebook.Utils.random_short_id()
    app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}

    Process.register(self(), test)

    input = %{
      type: :input,
      ref: "ref1",
      id: "input1",
      destination: test,
      attrs: %{type: :number, default: 1, label: "Name", debounce: :blur}
    }

    id = Livebook.Utils.random_short_id() |> String.to_atom()

    notebook = %{
      Livebook.Notebook.new()
      | app_settings: app_settings,
        sections: [
          %{
            Livebook.Notebook.Section.new()
            | cells: [
                %{
                  Livebook.Notebook.Cell.new(:code)
                  | source: source_for_output(input)
                },
                %{
                  Livebook.Notebook.Cell.new(:code)
                  | source: source_for_input_read(input.id)
                },
                %{
                  Livebook.Notebook.Cell.new(:code)
                  | id: "error-cell",
                    source: """
                    # Fail on the first run
                    unless :persistent_term.get(#{inspect(id)}, false) do
                      :persistent_term.put(#{inspect(id)}, true)
                      raise "oops"
                    end
                    """
                }
              ]
          }
        ]
    }

    Livebook.Apps.subscribe()
    app_pid = deploy_notebook_sync(notebook)

    assert_receive {:app_created, %{pid: ^app_pid} = app}

    assert_receive {:app_updated,
                    %{
                      pid: ^app_pid,
                      sessions: [%{pid: session_pid, app_status: %{execution: :error}}]
                    }}

    Livebook.Session.set_input_value(session_pid, input.id, 10)

    {:ok, view, _} = conn |> live(~p"/apps/#{slug}") |> follow_redirect(conn)

    # The button should not appear on error
    refute render(view) =~
             "Some inputs have changed.\nClick this button to process with latest values."

    view
    |> element("button", "Retry")
    |> render_click()

    assert_receive {:app_updated,
                    %{pid: ^app_pid, sessions: [%{app_status: %{execution: :executed}}]}}

    Livebook.Session.set_input_value(session_pid, input.id, 20)
    Livebook.SessionHelpers.wait_for_session_update(session_pid)

    assert render(view) =~
             "Some inputs have changed.\nClick this button to process with latest values."

    Livebook.App.close(app.pid)
  end
end
