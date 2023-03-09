defmodule LivebookWeb.Hub.NewLiveTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Livebook.Hubs

  test "render hub selection cards", %{conn: conn} do
    {:ok, _view, html} = live(conn, ~p"/hub")

    assert html =~ "Fly"
    assert html =~ "Livebook Teams"
  end

  describe "fly" do
    test "persists new hub", %{conn: conn} do
      fly_bypass("123456789")

      {:ok, view, _html} = live(conn, ~p"/hub")

      assert view
             |> element("#fly")
             |> render_click() =~ "2. Configure your Hub"

      assert view
             |> element(~s/input[name="fly[access_token]"]/)
             |> render_change(%{"fly" => %{"access_token" => "dummy access token"}}) =~
               ~s(<option value="123456789">Foo Bar - 123456789</option>)

      attrs = %{
        "access_token" => "dummy access token",
        "application_id" => "123456789",
        "hub_name" => "My Foo Hub",
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

      assert render(view) =~ "Hub added successfully"

      hubs_html = view |> element("#hubs") |> render()

      assert hubs_html =~ "🐈"
      assert hubs_html =~ "/hub/fly-123456789"
      assert hubs_html =~ "My Foo Hub"
    end

    test "fails to create existing hub", %{conn: conn} do
      hub = insert_hub(:fly, id: "fly-foo", application_id: "foo")
      fly_bypass(hub.application_id)

      {:ok, view, _html} = live(conn, ~p"/hub")

      assert view
             |> element("#fly")
             |> render_click() =~ "2. Configure your Hub"

      assert view
             |> element(~s/input[name="fly[access_token]"]/)
             |> render_change(%{"fly" => %{"access_token" => "dummy access token"}}) =~
               ~s(<option value="foo">Foo Bar - foo</option>)

      attrs = %{
        "access_token" => "dummy access token",
        "application_id" => "foo",
        "hub_name" => "My Foo Hub",
        "hub_emoji" => "🐈"
      }

      view
      |> element("#fly-form")
      |> render_change(%{"fly" => attrs})

      refute view
             |> element("#fly-form .invalid-feedback")
             |> has_element?()

      assert view
             |> element("#fly-form")
             |> render_submit(%{"fly" => attrs}) =~ "already exists"

      assert_hub(view, hub)
      assert Hubs.fetch_hub!(hub.id) == hub
    end
  end

  defp fly_bypass(app_id) do
    bypass = Bypass.open()
    Application.put_env(:livebook, :fly_graphql_endpoint, "http://localhost:#{bypass.port}")

    Bypass.expect(bypass, "POST", "/", fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)
      body = Jason.decode!(body)

      response =
        cond do
          body["query"] =~ "apps" -> fetch_apps_response(app_id)
          body["query"] =~ "app" -> fetch_app_response(app_id)
        end

      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.resp(200, Jason.encode!(response))
    end)
  end

  defp fetch_apps_response(app_id) do
    app = %{
      "id" => app_id,
      "organization" => %{
        "id" => "l3soyvjmvtmwtl6l2drnbfuvltipprge",
        "name" => "Foo Bar",
        "type" => "PERSONAL"
      }
    }

    %{"data" => %{"apps" => %{"nodes" => [app]}}}
  end

  defp fetch_app_response(app_id) do
    app = %{
      "id" => app_id,
      "name" => app_id,
      "hostname" => app_id <> ".fly.dev",
      "platformVersion" => "nomad",
      "deployed" => true,
      "status" => "running",
      "secrets" => [
        %{
          "createdAt" => to_string(DateTime.utc_now()),
          "digest" => to_string(Livebook.Utils.random_cookie()),
          "id" => Livebook.Utils.random_short_id(),
          "name" => "LIVEBOOK_PASSWORD"
        },
        %{
          "createdAt" => to_string(DateTime.utc_now()),
          "digest" => to_string(Livebook.Utils.random_cookie()),
          "id" => Livebook.Utils.random_short_id(),
          "name" => "LIVEBOOK_SECRET_KEY_BASE"
        }
      ]
    }

    %{"data" => %{"app" => app}}
  end

  defp assert_hub(view, hub) do
    hubs_html = view |> element("#hubs") |> render()

    assert hubs_html =~ hub.hub_emoji
    assert hubs_html =~ ~p"/hub/#{hub.id}"
    assert hubs_html =~ hub.hub_name
  end
end
