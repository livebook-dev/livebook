defmodule LivebookWeb.SessionLive.SecretsComponentTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.Secrets.Secret
  alias Livebook.Session
  alias Livebook.Sessions

  describe "enterprise" do
    setup %{url: url, token: token} do
      id = Livebook.Utils.random_id()
      Livebook.Hubs.delete_hub("enterprise-#{id}")

      enterprise =
        insert_hub(:enterprise,
          id: "enterprise-#{id}",
          external_id: id,
          url: url,
          token: token
        )

      {:ok, session} = Sessions.create_session(notebook: Livebook.Notebook.new())
      Livebook.Hubs.EnterpriseClient.subscribe()
      Livebook.Hubs.connect_hubs()

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
      {:ok, view, _html} = live(conn, Routes.session_path(conn, :secrets, session.id))

      assert view
             |> element(~s{form[phx-submit="save"]})
             |> render_change(%{
               data: %{
                 name: "FOO",
                 value: "123",
                 store: "hub"
               }
             }) =~ ~s(<option value="#{enterprise.id}">#{enterprise.hub_name}</option>)
    end

    test "creates a secret on Enterprise hub", %{
      conn: conn,
      session: session,
      enterprise: enterprise
    } do
      {:ok, view, _html} = live(conn, Routes.session_path(conn, :secrets, session.id))

      attrs = %{
        data: %{
          name: "FOO",
          value: "123",
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

      assert_receive {:secret_created, %Secret{name: "FOO", value: "123"}}
      assert render(view) =~ "A new secret has been created on your Livebook Enterprise"
      assert has_element?(view, "#enterprise-secret-#{attrs.data.name}-title")
    end
  end
end
