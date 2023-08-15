defmodule LivebookWeb.Hub.EditLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.HubHelpers
  import Livebook.TestHelpers

  alias Livebook.Hubs

  describe "personal" do
    setup do
      Livebook.Hubs.subscribe([:crud, :secrets])
      {:ok, hub: Hubs.fetch_hub!(Hubs.Personal.id())}
    end

    test "updates the hub", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{"hub_emoji" => "🐈"}

      view
      |> element("#personal-form")
      |> render_change(%{"personal" => attrs})

      refute view
             |> element("#personal-form .invalid-feedback")
             |> has_element?()

      assert {:ok, view, _html} =
               view
               |> element("#personal-form")
               |> render_submit(%{"personal" => attrs})
               |> follow_redirect(conn)

      assert render(view) =~ "Hub updated successfully"

      id = hub.id
      assert_receive {:hub_changed, ^id}

      assert_sidebar_hub(view, id, hub.hub_name, attrs["hub_emoji"])
      refute Hubs.fetch_hub!(hub.id) == hub
    end

    test "raises an error if does not exist secret", %{conn: conn, hub: hub} do
      assert_raise LivebookWeb.NotFoundError, fn ->
        live(conn, ~p"/hub/#{hub.id}/secrets/edit/HELLO")
      end
    end

    test "creates secret", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      secret = build(:secret, name: "PERSONAL_ADD_SECRET")

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

      %{"success" => "Secret PERSONAL_ADD_SECRET created successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert secret in Livebook.Hubs.get_secrets(hub)
    end

    test "updates secret", %{conn: conn, hub: hub} do
      secret = insert_secret(name: "PERSONAL_EDIT_SECRET", value: "GetTheBonk")

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

      %{"success" => "Secret PERSONAL_EDIT_SECRET updated successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      assert render(element(view, "#hub-secrets-list")) =~ secret.name
      assert updated_secret in Livebook.Hubs.get_secrets(hub)
    end

    test "deletes secret", %{conn: conn, hub: hub} do
      secret = insert_secret(name: "PERSONAL_DELETE_SECRET", value: "GetTheBonk")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      refute view
             |> element("#secrets-form button[disabled]")
             |> has_element?()

      view
      |> element("#hub-secret-#{secret.name}-delete", "Delete")
      |> render_click()

      render_confirm(view)

      assert_receive {:secret_deleted, ^secret}

      %{"success" => "Secret PERSONAL_DELETE_SECRET deleted successfully"} =
        assert_redirect(view, "/hub/#{hub.id}")

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      refute render(element(view, "#hub-secrets-list")) =~ secret.name
      refute secret in Livebook.Hubs.get_secrets(hub)
    end
  end
end
