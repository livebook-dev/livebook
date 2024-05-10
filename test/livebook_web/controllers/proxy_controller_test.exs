defmodule LivebookWeb.ProxyControllerTest do
  use LivebookWeb.ConnCase, async: true

  require Phoenix.LiveViewTest
  import Livebook.SessionHelpers

  alias Livebook.{Notebook, Runtime, Session, Sessions}

  setup do
    {:ok, req: Req.new(base_url: @endpoint.url(), retry: false)}
  end

  describe "session" do
    test "returns error when session doesn't exist", %{req: req} do
      session_id = Livebook.Utils.random_long_id()
      response = Req.get!(req, url: "/sessions/#{session_id}/proxy/foo/bar")

      assert response.status == 404
      assert response.body =~ "No Numbats here"
    end

    test "returns error when runtime is disconnected", %{req: req} do
      {:ok, session} = Sessions.create_session()
      response = Req.get!(req, url: "/sessions/#{session.id}/proxy/foo/bar")

      assert response.status == 400
      assert response.body =~ "Something went wrong"
    end

    test "returns the proxied response defined in notebook", %{req: req} do
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
            |> Plug.Conn.put_resp_header("content-type", "text/plain")
            |> Plug.Conn.send_resp(200, "used " <> conn.method <> " method")
          end)
          """
        )

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      url = "/sessions/#{session.id}/proxy/"

      assert Req.get!(req, url: url).body == "used GET method"
      assert Req.post!(req, url: url).body == "used POST method"
      assert Req.put!(req, url: url).body == "used PUT method"
      assert Req.patch!(req, url: url).body == "used PATCH method"
      assert Req.delete!(req, url: url).body == "used DELETE method"

      Session.close(session.pid)
    end
  end

  describe "app" do
    test "returns error when app doesn't exist", %{req: req} do
      slug = Livebook.Utils.random_long_id()
      response = Req.get!(req, url: "/apps/#{slug}/proxy/foo/bar")

      assert response.status == 404
      assert response.body =~ "No Numbats here"
    end
  end
end
