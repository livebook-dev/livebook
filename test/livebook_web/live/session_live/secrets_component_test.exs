defmodule LivebookWeb.SessionLive.SecretsComponentTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.Session
  alias Livebook.Sessions

  describe "enterprise" do
    setup %{url: url, token: token} do
      id = Livebook.Utils.random_short_id()
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

    test "creates a secret on Enterprise hub",
         %{conn: conn, session: session, enterprise: enterprise} do
      id = enterprise.id
      secret = build(:secret, name: "BIG_IMPORTANT_SECRET", value: "123", origin: {:hub, id})
      {:ok, view, _html} = live(conn, Routes.session_path(conn, :secrets, session.id))

      attrs = %{
        data: %{
          name: secret.name,
          value: secret.value,
          store: "hub",
          hub_id: enterprise.id
        }
      }

      form = element(view, ~s{form[phx-submit="save"]})
      render_change(form, attrs)
      render_submit(form, attrs)

      assert_receive {:secret_created, ^secret}
      assert render(view) =~ "A new secret has been created on your Livebook Enterprise"
      assert has_element?(view, "#hub-#{enterprise.id}-secret-#{attrs.data.name}-title")

      assert has_element?(
               view,
               "#hub-#{enterprise.id}-secret-#{secret.name}-title span",
               enterprise.hub_emoji
             )
    end

    test "toggle a secret from Enterprise hub",
         %{conn: conn, session: session, enterprise: enterprise, node: node} do
      secret =
        build(:secret,
          name: "POSTGRES_PASSWORD",
          value: "postgres",
          origin: {:hub, enterprise.id}
        )

      {:ok, view, _html} = live(conn, Routes.session_path(conn, :page, session.id))

      :erpc.call(node, Enterprise.Integration, :create_secret, [secret.name, secret.value])
      assert_receive {:secret_created, ^secret}

      Session.set_secret(session.pid, secret)
      assert_session_secret(view, session.pid, secret)
    end

    defp assert_session_secret(view, session_pid, %{origin: {:hub, id}} = secret) do
      secrets = Session.get_data(session_pid).secrets

      assert has_element?(view, "#hub-#{id}-secret-#{secret.name}-title")
      assert Map.has_key?(secrets, secret.name)
      assert secrets[secret.name] == secret.value
    end
  end
end
