defmodule LivebookWeb.SessionLive.SecretsComponentTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  import Livebook.SessionHelpers
  import Phoenix.LiveViewTest

  alias Livebook.Session
  alias Livebook.Sessions

  describe "enterprise" do
    setup %{test: name} do
      start_new_instance(name)

      node = EnterpriseServer.get_node(name)
      url = EnterpriseServer.url(name)
      token = EnterpriseServer.token(name)

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
        Livebook.Hubs.delete_hub(hub_id)
        Session.close(session.pid)
        stop_new_instance(name)
      end)

      {:ok, enterprise: enterprise, session: session, node: node}
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

    test "adding a missing secret using 'Add secret' button",
         %{conn: conn, session: session, enterprise: enterprise} do
      secret =
        build(:secret,
          name: "PGPASS",
          value: "postgres",
          origin: {:hub, enterprise.id}
        )

      Session.subscribe(session.id)
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      {:ok, view, _} = live(conn, "/sessions/#{session.id}")

      expected_url = Routes.session_path(conn, :secrets, session.id, secret_name: secret.name)
      add_secret_button = element(view, "a[href='#{expected_url}']")

      assert has_element?(add_secret_button)
      render_click(add_secret_button)

      secrets_component = with_target(view, "#secrets-modal")
      form_element = element(secrets_component, "form[phx-submit='save']")

      assert has_element?(form_element)

      render_submit(form_element, %{
        data: %{
          value: secret.value,
          store: "hub",
          hub_id: enterprise.id
        }
      })

      assert_receive {:secret_created, ^secret}
      assert_session_secret(view, session.pid, secret)
      refute secret in Livebook.Secrets.get_secrets()

      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, output}, _}}

      assert output == "\e[32m\"#{secret.value}\"\e[0m"
    end

    test "granting access for missing secret using 'Add secret' button",
         %{conn: conn, session: session, enterprise: enterprise, node: node} do
      secret =
        build(:secret,
          name: "MYSQL_PASS",
          value: "admin",
          origin: {:hub, enterprise.id}
        )

      Session.subscribe(session.id)
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      {:ok, view, _} = live(conn, "/sessions/#{session.id}")

      expected_url = Routes.session_path(conn, :secrets, session.id, secret_name: secret.name)
      add_secret_button = element(view, "a[href='#{expected_url}']")

      :erpc.call(node, Enterprise.Integration, :create_secret, [secret.name, secret.value])
      assert_receive {:secret_created, ^secret}, 10_000

      assert has_element?(add_secret_button)
      assert secret in Livebook.Hubs.get_secrets()

      render_click(add_secret_button)
      secrets_component = with_target(view, "#secrets-modal")

      assert render(secrets_component) =~ "in your Livebook Hub. Allow this session to access it?"

      grant_access_button = element(secrets_component, "button", "Grant access")
      render_click(grant_access_button)

      assert_session_secret(view, session.pid, secret)

      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, output}, _}}

      assert output == "\e[32m\"#{secret.value}\"\e[0m"
    end
  end
end
