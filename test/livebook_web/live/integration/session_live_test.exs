defmodule LivebookWeb.Integration.SessionLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.{Sessions, Session}

  setup do
    {:ok, session} = Sessions.create_session(notebook: Livebook.Notebook.new())

    on_exit(fn ->
      Session.close(session.pid)
    end)

    %{session: session}
  end

  describe "hubs" do
    test "selects the notebook hub", %{conn: conn, user: user, node: node, session: session} do
      org = :erpc.call(node, Hub.Integration, :create_org, [])
      org_key = :erpc.call(node, Hub.Integration, :create_org_key, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      hub =
        insert_hub(:team,
          id: "team-#{org.name}",
          hub_name: org.name,
          user_id: user.id,
          org_id: org.id,
          org_key_id: org_key.id,
          session_token: token
        )

      id = hub.id
      personal_id = Livebook.Hubs.Personal.id()

      Session.subscribe(session.id)
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert Session.get_data(session.pid).notebook.hub_id == personal_id

      view
      |> element(~s/#select-hub-#{id}/)
      |> render_click()

      assert_receive {:operation, {:set_notebook_hub, _, ^id}}
      assert Session.get_data(session.pid).notebook.hub_id == hub.id
    end
  end
end
