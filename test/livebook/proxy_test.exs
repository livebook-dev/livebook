defmodule Livebook.ProxyTest do
  use LivebookWeb.ConnCase, async: true

  require Phoenix.LiveViewTest
  import Livebook.AppHelpers

  alias Livebook.{Notebook, Runtime, Session, Sessions}

  describe "session" do
    test "returns error when session doesn't exist", %{conn: conn} do
      session_id = Livebook.Utils.random_long_id()

      assert_error_sent 404, fn ->
        get(conn, "/sessions/#{session_id}/proxy/foo/bar")
      end
    end

    test "returns error when runtime is disconnected", %{conn: conn} do
      {:ok, session} = Sessions.create_session()

      assert_error_sent 404, fn ->
        get(conn, "/sessions/#{session.id}/proxy/foo/bar")
      end
    end

    test "returns the proxied response defined in notebook", %{conn: conn} do
      %{sections: [%{cells: [%{id: cell_id}]}]} = notebook = proxy_notebook()
      {:ok, session} = Sessions.create_session(notebook: notebook)
      {:ok, runtime} = Runtime.Embedded.new() |> Runtime.connect()

      Session.set_runtime(session.pid, runtime)
      Session.subscribe(session.id)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, _, %{errored: false}}},
                     4_000

      url = "/sessions/#{session.id}/proxy/"

      assert text_response(get(conn, url), 200) == "used GET method"
      assert text_response(post(conn, url), 200) == "used POST method"
      assert text_response(put(conn, url), 200) == "used PUT method"
      assert text_response(patch(conn, url), 200) == "used PATCH method"
      assert text_response(delete(conn, url), 200) == "used DELETE method"

      Session.close(session.pid)
    end
  end

  describe "app" do
    test "returns error when app doesn't exist", %{conn: conn} do
      slug = Livebook.Utils.random_short_id()
      session_id = Livebook.Utils.random_long_id()

      assert_error_sent 404, fn ->
        get(conn, "/apps/#{slug}/#{session_id}/proxy/foo/bar")
      end
    end

    test "returns the proxied response defined in notebook", %{conn: conn} do
      slug = Livebook.Utils.random_short_id()
      app_settings = %{Notebook.AppSettings.new() | slug: slug, access_type: :public}
      notebook = %{proxy_notebook() | app_settings: app_settings}

      Livebook.Apps.subscribe()
      pid = deploy_notebook_sync(notebook)

      assert_receive {:app_created, %{pid: ^pid, slug: ^slug, sessions: []}}

      assert_receive {:app_updated,
                      %{slug: ^slug, sessions: [%{id: id, app_status: %{execution: :executed}}]}}

      url = "/apps/#{slug}/#{id}/proxy/"

      assert text_response(get(conn, url), 200) == "used GET method"
      assert text_response(post(conn, url), 200) == "used POST method"
      assert text_response(put(conn, url), 200) == "used PUT method"
      assert text_response(patch(conn, url), 200) == "used PATCH method"
      assert text_response(delete(conn, url), 200) == "used DELETE method"
    end
  end

  defp proxy_notebook() do
    cell =
      %{
        Notebook.Cell.new(:code)
        | source: """
          fun = fn conn ->
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/text;charset=utf-8")
            |> Plug.Conn.send_resp(200, "used " <> conn.method <> " method")
          end

          ref = make_ref()
          request = {:livebook_get_proxy_handler_child_spec, fun}
          send(Process.group_leader(), {:io_request, self(), ref, request})

          child_spec =
            receive do
              {:io_reply, ^ref, child_spec} -> child_spec
            end

          Supervisor.start_link([child_spec], strategy: :one_for_one)\
          """
      }

    section = %{Notebook.Section.new() | cells: [cell]}
    %{Notebook.new() | sections: [section]}
  end
end
