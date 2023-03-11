defmodule LivebookWeb.Hub.EditLiveTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Livebook.Hubs

  describe "fly" do
    setup do
      bypass = Bypass.open()
      Application.put_env(:livebook, :fly_graphql_endpoint, "http://localhost:#{bypass.port}")

      {:ok, bypass: bypass}
    end

    test "updates hub", %{conn: conn, bypass: bypass} do
      {:ok, pid} = Agent.start(fn -> %{fun: &fetch_app_response/2, type: :mount} end)

      app_id = Livebook.Utils.random_short_id()
      hub = insert_hub(:fly, id: "fly-#{app_id}", application_id: app_id)
      fly_bypass(bypass, app_id, pid)

      {:ok, view, html} = live(conn, ~p"/hub/#{hub.id}")

      assert html =~ "Manage app on Fly"
      assert html =~ "https://fly.io/apps/#{hub.application_id}"

      assert html =~ "Environment Variables"
      refute html =~ "FOO_ENV_VAR"
      assert html =~ "LIVEBOOK_PASSWORD"
      assert html =~ "LIVEBOOK_SECRET_KEY_BASE"

      attrs = %{
        "hub_name" => "Personal Hub",
        "hub_emoji" => "🐈"
      }

      view
      |> element("#fly-form")
      |> render_change(%{"fly" => attrs})

      refute view
             |> element("#fly-form .invalid-feedback")
             |> has_element?()

      assert {:ok, view, _html} =
               view
               |> element("#fly-form")
               |> render_submit(%{"fly" => attrs})
               |> follow_redirect(conn)

      assert render(view) =~ "Hub updated successfully"

      assert_hub(view, %{hub | hub_emoji: attrs["hub_emoji"], hub_name: attrs["hub_name"]})
      refute Hubs.fetch_hub!(hub.id) == hub
    end

    test "deletes hub", %{conn: conn, bypass: bypass} do
      {:ok, pid} = Agent.start(fn -> %{fun: &fetch_app_response/2, type: :mount} end)
      app_id = Livebook.Utils.random_short_id()
      hub_id = "fly-#{app_id}"

      hub = insert_hub(:fly, id: hub_id, hub_name: "My Deletable Hub", application_id: app_id)
      fly_bypass(bypass, app_id, pid)

      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      assert {:ok, view, _html} =
               view
               |> render_click("delete_hub", %{"id" => hub_id})
               |> follow_redirect(conn)

      hubs_html = view |> element("#hubs") |> render()

      refute hubs_html =~ ~p"/hub/#{hub.id}"
      refute hubs_html =~ hub.hub_name

      assert Hubs.fetch_hub(hub_id) == :error
    end

    test "add env var", %{conn: conn, bypass: bypass} do
      {:ok, pid} = Agent.start(fn -> %{fun: &fetch_app_response/2, type: :mount} end)

      app_id = Livebook.Utils.random_short_id()
      hub = insert_hub(:fly, id: "fly-#{app_id}", application_id: app_id)
      fly_bypass(bypass, app_id, pid)

      {:ok, view, html} = live(conn, ~p"/hub/#{hub.id}")

      assert html =~ "Manage app on Fly"
      assert html =~ "https://fly.io/apps/#{hub.application_id}"

      assert html =~ "Environment Variables"
      refute html =~ "FOO_ENV_VAR"
      assert html =~ "LIVEBOOK_PASSWORD"
      assert html =~ "LIVEBOOK_SECRET_KEY_BASE"

      view
      |> element("#add-env-var")
      |> render_click(%{})

      assert_patch(view, ~p"/hub/#{hub.id}/env-var/new")
      assert render(view) =~ "Add environment variable"

      attrs = params_for(:env_var, name: "FOO_ENV_VAR")

      view
      |> element("#env-var-form")
      |> render_change(%{"env_var" => attrs})

      refute view
             |> element("#env-var-form button[disabled]")
             |> has_element?()

      :ok = Agent.update(pid, fn state -> %{state | type: :add} end)

      assert {:ok, _view, html} =
               view
               |> element("#env-var-form")
               |> render_submit(%{"env_var" => attrs})
               |> follow_redirect(conn)

      assert html =~ "Environment variable added"
      assert html =~ "Environment Variables"
      assert html =~ "FOO_ENV_VAR"
      assert html =~ "LIVEBOOK_PASSWORD"
      assert html =~ "LIVEBOOK_SECRET_KEY_BASE"
    end

    test "update env var", %{conn: conn, bypass: bypass} do
      {:ok, pid} = Agent.start(fn -> %{fun: &fetch_app_response/2, type: :foo} end)

      app_id = Livebook.Utils.random_short_id()
      hub = insert_hub(:fly, id: "fly-#{app_id}", application_id: app_id)
      fly_bypass(bypass, app_id, pid)

      {:ok, view, html} = live(conn, ~p"/hub/#{hub.id}")

      assert html =~ "Manage app on Fly"
      assert html =~ "https://fly.io/apps/#{hub.application_id}"

      assert html =~ "Environment Variables"
      assert html =~ "FOO_ENV_VAR"

      view
      |> element("#env-var-FOO_ENV_VAR-edit")
      |> render_click(%{"env_var" => "FOO_ENV_VAR"})

      assert_patch(view, ~p"/hub/#{hub.id}/env-var/edit/FOO_ENV_VAR")
      assert render(view) =~ "Edit environment variable"

      attrs = params_for(:env_var, name: "FOO_ENV_VAR")

      view
      |> element("#env-var-form")
      |> render_change(%{"env_var" => attrs})

      refute view
             |> element("#env-var-form button[disabled]")
             |> has_element?()

      :ok = Agent.update(pid, fn state -> %{state | type: :updated_foo} end)

      assert {:ok, _view, html} =
               view
               |> element("#env-var-form")
               |> render_submit(%{"env_var" => attrs})
               |> follow_redirect(conn)

      assert html =~ "Environment variable updated"
      assert html =~ "Environment Variables"
      assert html =~ "FOO_ENV_VAR"
    end

    test "delete env var", %{conn: conn, bypass: bypass} do
      {:ok, pid} = Agent.start(fn -> %{fun: &fetch_app_response/2, type: :add} end)

      app_id = Livebook.Utils.random_short_id()
      hub = insert_hub(:fly, id: "fly-#{app_id}", application_id: app_id)
      fly_bypass(bypass, app_id, pid)

      {:ok, view, html} = live(conn, ~p"/hub/#{hub.id}")

      assert html =~ "Manage app on Fly"
      assert html =~ "https://fly.io/apps/#{hub.application_id}"

      assert html =~ "Environment Variables"
      assert html =~ "FOO_ENV_VAR"
      assert html =~ "LIVEBOOK_PASSWORD"
      assert html =~ "LIVEBOOK_SECRET_KEY_BASE"

      :ok = Agent.update(pid, fn state -> %{state | type: :mount} end)

      assert {:ok, _view, html} =
               view
               |> with_target("#fly-form-component")
               |> render_click("delete_env_var", %{"env_var" => "FOO_ENV_VAR"})
               |> follow_redirect(conn)

      assert html =~ "Environment variable deleted"
      assert html =~ "Environment Variables"
      refute html =~ "FOO_ENV_VAR"
      assert html =~ "LIVEBOOK_PASSWORD"
      assert html =~ "LIVEBOOK_SECRET_KEY_BASE"
    end
  end

  describe "enterprise" do
    test "updates hub", %{conn: conn} do
      hub = insert_hub(:enterprise)
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{"hub_emoji" => "🐈"}

      view
      |> element("#enterprise-form")
      |> render_change(%{"enterprise" => attrs})

      refute view
             |> element("#enterprise-form .invalid-feedback")
             |> has_element?()

      assert {:ok, view, _html} =
               view
               |> element("#enterprise-form")
               |> render_submit(%{"enterprise" => attrs})
               |> follow_redirect(conn)

      assert render(view) =~ "Hub updated successfully"

      assert_hub(view, %{hub | hub_emoji: attrs["hub_emoji"]})
      refute Hubs.fetch_hub!(hub.id) == hub
    end
  end

  describe "personal" do
    setup do
      Livebook.Hubs.subscribe([:secrets])
      {:ok, hub: Hubs.fetch_hub!(Hubs.Personal.id())}
    end

    test "updates the hub", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")

      attrs = %{"hub_emoji" => "🐈"}

      view
      |> element("#personal-form")
      |> render_change(%{"personal" => attrs})

      refute view
             |> element("#enterprise-form .invalid-feedback")
             |> has_element?()

      assert {:ok, view, _html} =
               view
               |> element("#personal-form")
               |> render_submit(%{"personal" => attrs})
               |> follow_redirect(conn)

      assert render(view) =~ "Hub updated successfully"

      assert_hub(view, %{hub | hub_emoji: attrs["hub_emoji"]})
      refute Hubs.fetch_hub!(hub.id) == hub
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
      assert render(view) =~ "Secret created successfully"
      assert render(view) =~ secret.name
      assert secret in Livebook.Hubs.get_secrets(hub)
    end

    test "updates secret", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      secret = insert_secret(name: "PERSONAL_EDIT_SECRET", value: "GetTheBonk")

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
      assert render(view) =~ "Secret updated successfully"
      assert render(view) =~ secret.name
      assert updated_secret in Livebook.Hubs.get_secrets(hub)
    end

    test "deletes secret", %{conn: conn, hub: hub} do
      {:ok, view, _html} = live(conn, ~p"/hub/#{hub.id}")
      secret = insert_secret(name: "PERSONAL_DELETE_SECRET", value: "GetTheBonk")

      refute view
             |> element("#secrets-form button[disabled]")
             |> has_element?()

      view
      |> with_target("#personal-form-component")
      |> render_click("delete_hub_secret", %{
        name: secret.name,
        value: secret.value,
        hub_id: secret.hub_id
      })

      assert_receive {:secret_deleted, ^secret}
      assert render(view) =~ "Secret deleted successfully"
      refute render(view) =~ secret.name
      refute secret in Livebook.Hubs.get_secrets(hub)
    end
  end

  defp assert_hub(view, hub) do
    hubs_html = view |> element("#hubs") |> render()

    assert hubs_html =~ hub.hub_emoji
    assert hubs_html =~ ~p"/hub/#{hub.id}"
    assert hubs_html =~ hub.hub_name
  end

  defp fly_bypass(bypass, app_id, agent_pid) do
    Bypass.expect(bypass, "POST", "/", fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)
      body = Jason.decode!(body)

      response =
        cond do
          body["query"] =~ "setSecrets" ->
            set_secrets_response()

          body["query"] =~ "unsetSecrets" ->
            unset_secrets_response()

          true ->
            Agent.get(agent_pid, fn
              %{fun: fun, type: type} -> fun.(app_id, type)
              %{fun: fun} -> fun.()
            end)
        end

      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.resp(200, Jason.encode!(response))
    end)
  end

  defp fetch_app_response(app_id, type) do
    app = %{
      "id" => app_id,
      "name" => app_id,
      "hostname" => app_id <> ".fly.dev",
      "platformVersion" => "nomad",
      "deployed" => true,
      "status" => "running",
      "secrets" => secrets(type)
    }

    %{"data" => %{"app" => app}}
  end

  defp secrets(:mount) do
    [
      %{
        "createdAt" => to_string(DateTime.utc_now()),
        "digest" => to_string(Livebook.Utils.random_cookie()),
        "id" => "123",
        "name" => "LIVEBOOK_PASSWORD"
      },
      %{
        "createdAt" => to_string(DateTime.utc_now()),
        "digest" => to_string(Livebook.Utils.random_cookie()),
        "id" => "456",
        "name" => "LIVEBOOK_SECRET_KEY_BASE"
      }
    ]
  end

  defp secrets(:add) do
    [
      %{
        "createdAt" => to_string(DateTime.utc_now()),
        "digest" => to_string(Livebook.Utils.random_cookie()),
        "id" => "789",
        "name" => "FOO_ENV_VAR"
      },
      %{
        "createdAt" => to_string(DateTime.utc_now()),
        "digest" => to_string(Livebook.Utils.random_cookie()),
        "id" => "123",
        "name" => "LIVEBOOK_PASSWORD"
      },
      %{
        "createdAt" => to_string(DateTime.utc_now()),
        "digest" => to_string(Livebook.Utils.random_cookie()),
        "id" => "456",
        "name" => "LIVEBOOK_SECRET_KEY_BASE"
      }
    ]
  end

  defp secrets(:foo) do
    [
      %{
        "createdAt" => "2022-08-31 14:47:39.904338Z",
        "digest" => to_string(Livebook.Utils.random_cookie()),
        "id" => "123456789",
        "name" => "FOO_ENV_VAR"
      }
    ]
  end

  defp secrets(:updated_foo) do
    [
      %{
        "createdAt" => "2022-08-31 14:47:41.632669Z",
        "digest" => to_string(Livebook.Utils.random_cookie()),
        "id" => "123456789",
        "name" => "FOO_ENV_VAR"
      }
    ]
  end

  defp set_secrets_response do
    %{"data" => %{"setSecrets" => %{"app" => %{"secrets" => secrets(:add)}}}}
  end

  defp unset_secrets_response do
    %{"data" => %{"unsetSecrets" => %{"app" => %{"secrets" => secrets(:mount)}}}}
  end
end
