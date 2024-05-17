defmodule LivebookWeb.ProxyPlugTest do
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
      cell = %{
        Notebook.Cell.new(:code)
        | source: """
          Kino.Proxy.listen(fn conn ->
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/text;charset=utf-8")
            |> Plug.Conn.send_resp(200, "used " <> conn.method <> " method")
          end)
          """
      }

      cell_id = cell.id

      section = %{Notebook.Section.new() | cells: [cell]}
      notebook = %{Notebook.new() | sections: [section]}

      {:ok, session} = Sessions.create_session(notebook: notebook)
      {:ok, runtime} = Runtime.Embedded.new() |> Runtime.connect()

      Session.set_runtime(session.pid, runtime)
      Session.subscribe(session.id)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

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

      cell = %{
        Notebook.Cell.new(:code)
        | source: """
          Kino.Proxy.listen(fn conn ->
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/text;charset=utf-8")
            |> Plug.Conn.send_resp(200, "used " <> conn.method <> " method")
          end)
          """
      }

      app_settings = %{Notebook.AppSettings.new() | slug: slug, access_type: :public}
      section = %{Notebook.Section.new() | cells: [cell]}
      notebook = %{Notebook.new() | app_settings: app_settings, sections: [section]}

      Livebook.Apps.subscribe()
      pid = deploy_notebook_sync(notebook)

      assert_receive {:app_created, %{pid: ^pid, slug: ^slug}}

      assert_receive {:app_updated,
                      %{
                        pid: ^pid,
                        slug: ^slug,
                        sessions: [
                          %{id: id, app_status: %{lifecycle: :active, execution: :executed}}
                        ]
                      }}

      url = "/apps/#{slug}/#{id}/proxy/"

      assert text_response(get(conn, url), 200) == "used GET method"
      assert text_response(post(conn, url), 200) == "used POST method"
      assert text_response(put(conn, url), 200) == "used PUT method"
      assert text_response(patch(conn, url), 200) == "used PATCH method"
      assert text_response(delete(conn, url), 200) == "used DELETE method"

      Livebook.App.close(pid)
    end
  end
end
