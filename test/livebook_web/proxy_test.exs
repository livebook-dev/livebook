defmodule LivebookWeb.ProxyTest do
  use LivebookWeb.ConnCase, async: true

  require Phoenix.LiveViewTest
  import Livebook.SessionHelpers

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
      notebook = %{Notebook.new() | name: "My Notebook"}

      {:ok, session} = Sessions.create_session(notebook: notebook)
      {:ok, runtime} = Runtime.Embedded.new() |> Runtime.connect()

      Session.set_runtime(session.pid, runtime)
      Session.subscribe(session.id)

      cell_id =
        insert_text_cell(
          session.pid,
          insert_section(session.pid),
          :code,
          """
          Kino.Proxy.listen(fn conn ->
            conn
            |> Plug.Conn.put_resp_header("content-type", "application/text;charset=utf-8")
            |> Plug.Conn.send_resp(200, "used " <> conn.method <> " method")
          end)
          """
        )

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
      slug = Livebook.Utils.random_long_id()

      assert_error_sent 404, fn ->
        get(conn, "/apps/#{slug}/proxy/foo/bar")
      end
    end

    @tag :tmp_dir
    test "returns the proxied response defined in notebook", %{conn: conn, tmp_dir: tmp_dir} do
      slug = Livebook.Utils.random_short_id()
      path = Path.join(tmp_dir, "my-notebook.livemd")

      File.write!(path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"}} -->

      # My Notebook

      ## Section 1

      ```elixir
      Kino.Proxy.listen(fn conn ->
        conn
        |> Plug.Conn.put_resp_header("content-type", "application/text;charset=utf-8")
        |> Plug.Conn.send_resp(200, "used " <> conn.method <> " method")
      end)
      ```
      """)

      Livebook.Apps.subscribe()
      assert [%{slug: ^slug} = app_spec] = Livebook.Apps.build_app_specs_in_dir(tmp_dir)

      deployer_pid = Livebook.Apps.Deployer.local_deployer()
      Livebook.Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

      assert_receive {:app_created, %{pid: pid, slug: ^slug}}
      assert_receive {:app_updated, %{slug: ^slug, sessions: [_session]}}

      url = "/apps/#{slug}/proxy/"

      assert text_response(get(conn, url), 200) == "used GET method"
      assert text_response(post(conn, url), 200) == "used POST method"
      assert text_response(put(conn, url), 200) == "used PUT method"
      assert text_response(patch(conn, url), 200) == "used PATCH method"
      assert text_response(delete(conn, url), 200) == "used DELETE method"

      Livebook.App.close(pid)
    end
  end
end
