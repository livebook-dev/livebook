defmodule LivebookWeb.SessionLive.SecretsComponentTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.Session
  alias Livebook.Sessions

  describe "enterprise" do
    setup %{url: url, token: token} do
      node = EnterpriseServer.get_node()
      id = :erpc.call(node, Enterprise.Integration, :fetch_env!, ["ENTERPRISE_ID"])
      hub_id = "enterprise-#{id}"

      Livebook.Hubs.subscribe([:connection, :secrets])
      Livebook.Hubs.delete_hub(hub_id)

      enterprise =
        insert_hub(:enterprise,
          id: hub_id,
          external_id: id,
          url: url,
          token: token
        )

      {:ok, session} = Sessions.create_session(notebook: Livebook.Notebook.new())

      on_exit(fn ->
        Session.close(session.pid)
      end)

      {:ok, enterprise: enterprise, session: session}
    end

    test "shows the connected hubs dropdown", %{
      conn: conn,
      session: session,
      enterprise: enterprise
    } do
      secret = build(:secret, name: "LESS_IMPORTANT_SECRET", value: "123", origin: enterprise.id)
      {:ok, view, _html} = live(conn, Routes.session_path(conn, :secrets, session.id))

      assert view
             |> element(~s{form[phx-submit="save"]})
             |> render_change(%{
               data: %{
                 name: secret.name,
                 value: secret.value,
                 store: "hub"
               }
             }) =~ ~s(<option value="#{enterprise.id}">#{enterprise.hub_name}</option>)
    end

    test "creates a secret on Enterprise hub", %{
      conn: conn,
      session: session,
      enterprise: enterprise
    } do
      id = enterprise.id
      secret = build(:secret, name: "BIG_IMPORTANT_SECRET", value: "123", origin: id)
      {:ok, view, _html} = live(conn, Routes.session_path(conn, :secrets, session.id))

      attrs = %{
        data: %{
          name: secret.name,
          value: secret.value,
          store: "hub",
          connected_hub: enterprise.id
        }
      }

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_change(attrs)

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(attrs)

      assert render(view) =~ "A new secret has been created on your Livebook Enterprise"
      assert has_element?(view, "#hub-secret-#{attrs.data.name}-title")
    end
  end
end
