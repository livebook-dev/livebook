defmodule LivebookWeb.Hub.EditLiveTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Livebook.Hubs

  setup do
    on_exit(fn ->
      Hubs.clean_hubs()
    end)

    bypass = Bypass.open()
    Application.put_env(:livebook, :fly_graphql_endpoint, "http://localhost:#{bypass.port}")

    {:ok, bypass: bypass}
  end

  describe "fly" do
    test "updates hub", %{conn: conn, bypass: bypass} do
      {:ok, pid} = Agent.start(fn -> %{fun: &fetch_app_response/2, type: :mount} end)

      app_id = Livebook.Utils.random_short_id()
      hub = insert_hub(:fly, id: "fly-#{app_id}", application_id: app_id)
      fly_bypass(bypass, app_id, pid)

      {:ok, view, html} = live(conn, Routes.hub_path(conn, :edit, hub.id))

      assert html =~ "Manage app on Fly"
      assert html =~ "https://fly.io/apps/#{hub.application_id}"

      assert html =~ "Environment Variables"
      refute html =~ "FOO_ENV_VAR"
      assert html =~ "LIVEBOOK_PASSWORD"
      assert html =~ "LIVEBOOK_SECRET_KEY_BASE"

      attrs = %{
        "hub_name" => "Personal Hub",
        "hub_color" => "#FF00FF"
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

      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #FF00FF"/

      assert view
             |> element("#hubs")
             |> render() =~ Routes.hub_path(conn, :edit, hub.id)

      assert view
             |> element("#hubs")
             |> render() =~ "Personal Hub"

      refute Hubs.fetch_hub!(hub.id) == hub
    end

    test "add env var", %{conn: conn, bypass: bypass} do
      {:ok, pid} = Agent.start(fn -> %{fun: &fetch_app_response/2, type: :mount} end)

      app_id = Livebook.Utils.random_short_id()
      hub = insert_hub(:fly, id: "fly-#{app_id}", application_id: app_id)
      fly_bypass(bypass, app_id, pid)

      {:ok, view, html} = live(conn, Routes.hub_path(conn, :edit, hub.id))

      assert html =~ "Manage app on Fly"
      assert html =~ "https://fly.io/apps/#{hub.application_id}"

      assert html =~ "Environment Variables"
      refute html =~ "FOO_ENV_VAR"
      assert html =~ "LIVEBOOK_PASSWORD"
      assert html =~ "LIVEBOOK_SECRET_KEY_BASE"

      view
      |> element("#add-env-var")
      |> render_click(%{})

      assert_patch(view, Routes.hub_path(conn, :add_env_var, hub.id))
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

      {:ok, view, html} = live(conn, Routes.hub_path(conn, :edit, hub.id))

      assert html =~ "Manage app on Fly"
      assert html =~ "https://fly.io/apps/#{hub.application_id}"

      assert html =~ "Environment Variables"
      assert html =~ "FOO_ENV_VAR"

      view
      |> element("#env-var-FOO_ENV_VAR-edit")
      |> render_click(%{"env_var" => "FOO_ENV_VAR"})

      assert_patch(view, Routes.hub_path(conn, :edit_env_var, hub.id, "FOO_ENV_VAR"))
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

      {:ok, view, html} = live(conn, Routes.hub_path(conn, :edit, hub.id))

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
