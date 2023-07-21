defmodule LivebookWeb.Integration.Hub.EditLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.Hubs

  setup %{user: user, node: node} do
    Livebook.Hubs.subscribe([:crud, :connection, :secrets])
    hub = create_team_hub(user, node)
    id = hub.id

    assert_receive {:hub_connected, ^id}

    {:ok, hub: hub}
  end

  describe "team" do
    test "updates the hub", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{"hub_emoji" => "🐈"}

      view
      |> element("#team-form")
      |> render_change(%{"team" => attrs})

      refute view
             |> element("#team-form .invalid-feedback")
             |> has_element?()

      assert {:ok, view, _html} =
               view
               |> element("#team-form")
               |> render_submit(%{"team" => attrs})
               |> follow_redirect(conn)

      assert render(view) =~ "Hub updated successfully"

      id = hub.id
      assert_receive {:hub_changed, ^id}

      assert_sidebar_hub(view, id, hub.hub_name, attrs["hub_emoji"])
      refute Hubs.fetch_hub!(hub.id) == hub
    end

    test "deletes the hub", %{conn: conn, hub: hub} do
      id = hub.id
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      view
      |> element("#delete-hub", "Delete hub")
      |> render_click()

      render_confirm(view)

      assert_receive {:hub_changed, ^id}
      %{"success" => "Hub deleted successfully"} = assert_redirect(view, "/")

      {:ok, view, _html} = live(conn, ~p"/")

      refute_sidebar_hub(view, id)
      assert_raise Livebook.Storage.NotFoundError, fn -> Hubs.fetch_hub!(id) end
    end

    test "creates a secret", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      secret = build(:secret, name: "TEAM_ADD_SECRET", hub_id: hub.id)

      attrs = %{
        secret: %{
          name: secret.name,
          value: secret.value,
          hub_id: secret.hub_id
        }
      }

      refute render(view) =~ secret.name

      view
      |> element("#add-secret")
      |> render_click(%{})

      assert_patch(view, ~p"/hub/#{hub.id}/secrets/new")
      assert render(view) =~ "Add secret"

      view
      |> element("#secrets-form")
      |> render_change(attrs)

      refute view
             |> element("#secrets-form button[disabled]")
             |> has_element?()

      view
      |> element("#secrets-form")
      |> render_submit(attrs)

      assert_receive {:secret_created, ^secret}
      %{"success" => "Secret created successfully"} = assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert secret in Livebook.Hubs.get_secrets(hub)
    end

    test "updates existing secret", %{conn: conn, hub: hub} do
      secret = insert_secret(name: "TEAM_EDIT_SECRET", hub_id: hub.id)
      assert_receive {:secret_created, ^secret}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{
        secret: %{
          name: secret.name,
          value: secret.value,
          hub_id: secret.hub_id
        }
      }

      new_value = "new_value"

      view
      |> element("#hub-secret-#{secret.name}-edit")
      |> render_click(%{"secret_name" => secret.name})

      assert_patch(view, ~p"/hub/#{hub.id}/secrets/edit/#{secret.name}")
      assert render(view) =~ "Edit secret"

      view
      |> element("#secrets-form")
      |> render_change(attrs)

      refute view
             |> element("#secrets-form button[disabled]")
             |> has_element?()

      view
      |> element("#secrets-form")
      |> render_submit(put_in(attrs.secret.value, new_value))

      updated_secret = %{secret | value: new_value}

      assert_receive {:secret_updated, ^updated_secret}
      %{"success" => "Secret updated successfully"} = assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert updated_secret in Livebook.Hubs.get_secrets(hub)
    end

    test "deletes existing secret", %{conn: conn, hub: hub} do
      secret = insert_secret(name: "TEAM_DELETE_SECRET", hub_id: hub.id)
      assert_receive {:secret_created, ^secret}

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      refute view
             |> element("#secrets-form button[disabled]")
             |> has_element?()

      view
      |> element("#hub-secret-#{secret.name}-delete", "Delete")
      |> render_click()

      render_confirm(view)

      assert_receive {:secret_deleted, ^secret}
      %{"success" => "Secret deleted successfully"} = assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      refute render(element(view, "#hub-secrets-list")) =~ secret.name
      refute secret in Livebook.Hubs.get_secrets(hub)
    end

    test "raises an error if does not exist secret", %{conn: conn, hub: hub} do
      assert_raise LivebookWeb.NotFoundError, fn ->
        live(conn, ~p"/hub/#{hub.id}/secrets/edit/HELLO")
      end
    end
  end

  describe "offline deployment" do
    test "shows the dockerfile with google iap identity provider", %{conn: conn, hub: hub} do
      identity_provider = Application.get_env(:livebook, :identity_provider)
      Application.put_env(:livebook, :identity_provider, {Livebook.ZTA.GoogleIAP, "livebook.dev"})

      bypass = Bypass.open()

      key = %{
        "kty" => "RSA",
        "e" => "AQAB",
        "use" => "sig",
        "kid" => "bmlyt6y2uWrgWeUh3mENiSkEOR7Np3I8swSjlK98iX0",
        "alg" => "RS256",
        "n" =>
          "qvMgmj7GrjMAKxib9ODcdNyMwhsU1jwjvyAANrCJ5n1UcM82lZ5B3YP13zbPY3vRuufkW_GuA2cEZ8htMGT79kMsPz1cLrwIeUNOdGzncQQvBJVmQgw8NOuflVy5OajvfSe4a5PQmpC6BEp1d-Ix0S4BV2vWJUb0UtHg3bM4GgHTrnhHkSyXfpSZT4SNqnSOtiXiD-7lue52cPlZotkeTR2D4LTVSrsCdp21wGvAxXqnfpRcKYs5EyEmyTQ85zak7nBAReMrAqrRilXej8qTWGGIg1TRILvoCMd3nF5QjcjRCx2JMMHXG4tZNoK4QbEQlsdcd45B1VpE15TwgNTx4Q"
      }

      token =
        "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiZW1haWwiOiJ0dWthQHBlcmFsdGEuY29tIiwiaWF0IjoxNTE2MjM5MDIyLCJpc3MiOiJsaXZlYm9vayIsImF1ZCI6ImxpdmVib29rd2ViIn0.ZP5LIrkfMHq2p8g3SMgC7RBt7899GeHkP9rzYKM3RvjDCBeYoxpeioLW2sXMT74QyJPxB4JUujRU3shSPIWNAxkjJVaBGwVTqsO_PR34DSx82q45qSkUnkSVXLl-2KgN4BoUUK7dmocP6yzhNQ3XGf6669n5UG69eMZdh9PApZ7GuyRUQ80ubpvakWaIpd9PIaORkptqDWVbyOIk3Z79AMUub0MSG1FpzYByAQoLswob24l2xVo95-aQrdatqLk1sJ43AZ6HLoMxkZkWobYYRMH5w65MkQckJ9NzI3Rk-VOUlg9ePo8OPRnvcGY-OozHXrjdzn2-j03xuP6x1J3Y7Q"

      options = [
        name: LivebookWeb.ZTA,
        custom_identity: %{
          iss: "livebook",
          key: "livebookweb",
          certs: "http://localhost:#{bypass.port}"
        }
      ]

      Bypass.expect(bypass, fn conn ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{keys: [key]}))
      end)

      conn = put_req_header(conn, "x-goog-iap-jwt-assertion", token)
      start_supervised!({Livebook.ZTA.GoogleIAP, options})

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      assert render(view) =~ "ENV LIVEBOOK_IDENTITY_PROVIDER &quot;google_iap:livebook.dev&quot;"
      Application.put_env(:livebook, :identity_provider, identity_provider)
    end

    test "shows the dockerfile with cloudflare identity provider", %{conn: conn, hub: hub} do
      identity_provider = Application.get_env(:livebook, :identity_provider)

      Application.put_env(
        :livebook,
        :identity_provider,
        {Livebook.ZTA.Cloudflare, "livebook"}
      )

      bypass = Bypass.open()
      user_identity = Bypass.open()

      key = %{
        "kty" => "RSA",
        "e" => "AQAB",
        "use" => "sig",
        "kid" => "bmlyt6y2uWrgWeUh3mENiSkEOR7Np3I8swSjlK98iX0",
        "alg" => "RS256",
        "n" =>
          "qvMgmj7GrjMAKxib9ODcdNyMwhsU1jwjvyAANrCJ5n1UcM82lZ5B3YP13zbPY3vRuufkW_GuA2cEZ8htMGT79kMsPz1cLrwIeUNOdGzncQQvBJVmQgw8NOuflVy5OajvfSe4a5PQmpC6BEp1d-Ix0S4BV2vWJUb0UtHg3bM4GgHTrnhHkSyXfpSZT4SNqnSOtiXiD-7lue52cPlZotkeTR2D4LTVSrsCdp21wGvAxXqnfpRcKYs5EyEmyTQ85zak7nBAReMrAqrRilXej8qTWGGIg1TRILvoCMd3nF5QjcjRCx2JMMHXG4tZNoK4QbEQlsdcd45B1VpE15TwgNTx4Q"
      }

      token =
        "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiZW1haWwiOiJ0dWthQHBlcmFsdGEuY29tIiwiaWF0IjoxNTE2MjM5MDIyLCJpc3MiOiJsaXZlYm9vayIsImF1ZCI6ImxpdmVib29rd2ViIn0.ZP5LIrkfMHq2p8g3SMgC7RBt7899GeHkP9rzYKM3RvjDCBeYoxpeioLW2sXMT74QyJPxB4JUujRU3shSPIWNAxkjJVaBGwVTqsO_PR34DSx82q45qSkUnkSVXLl-2KgN4BoUUK7dmocP6yzhNQ3XGf6669n5UG69eMZdh9PApZ7GuyRUQ80ubpvakWaIpd9PIaORkptqDWVbyOIk3Z79AMUub0MSG1FpzYByAQoLswob24l2xVo95-aQrdatqLk1sJ43AZ6HLoMxkZkWobYYRMH5w65MkQckJ9NzI3Rk-VOUlg9ePo8OPRnvcGY-OozHXrjdzn2-j03xuP6x1J3Y7Q"

      options = [
        name: LivebookWeb.ZTA,
        custom_identity: %{
          iss: "livebook",
          key: "livebookweb",
          certs: "http://localhost:#{bypass.port}",
          user_identity: "http://localhost:#{user_identity.port}"
        }
      ]

      Bypass.expect(bypass, fn conn ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{keys: [key]}))
      end)

      expected_user = %{
        "user_uuid" => "1234567890",
        "name" => "Tuka Peralta",
        "email" => "tuka@peralta.com"
      }

      Bypass.expect_once(user_identity, fn conn ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(expected_user))
      end)

      conn = put_req_header(conn, "cf-access-jwt-assertion", token)
      start_supervised!({Livebook.ZTA.Cloudflare, options})

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      assert render(view) =~ "ENV LIVEBOOK_IDENTITY_PROVIDER &quot;cloudflare:livebook&quot;"
      Application.put_env(:livebook, :identity_provider, identity_provider)
    end
  end
end
