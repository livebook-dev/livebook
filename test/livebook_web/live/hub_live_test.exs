defmodule LivebookWeb.HubLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Livebook.Fixtures
  import Phoenix.LiveViewTest

  alias Livebook.Hubs

  setup do
    on_exit(&Hubs.clean_hubs/0)

    :ok
  end

  test "render hub selection cards", %{conn: conn} do
    {:ok, _view, html} = live(conn, "/hub")

    assert html =~ "Fly"
    assert html =~ "Livebook Enterprise"
  end

  describe "fly" do
    test "persists fly", %{conn: conn} do
      fly_app_bypass("123456789")

      {:ok, view, _html} = live(conn, "/hub")

      # renders the second step
      assert view
             |> element("#fly")
             |> render_click() =~ "2. Configure your Hub"

      # triggers the access_access_token field change
      # and shows the fly's third step
      assert view
             |> element(~s/input[name="fly[access_token]"]/)
             |> render_change(%{"fly" => %{"access_token" => "dummy access token"}}) =~
               ~s(<option value="123456789">Foo Bar - 123456789</option>)

      # triggers the application_id field change
      # and assigns `selected_app` to socket
      view
      |> element(~s/select[name="fly[application_id]"]/)
      |> render_change(%{"fly" => %{"application_id" => "123456789"}})

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      refute view
             |> element("#fly-form")
             |> render_submit(%{
               "fly" => %{
                 "access_token" => "dummy access token",
                 "application_id" => "123456789",
                 "hub_name" => "My Foo Hub",
                 "hub_color" => "#FF00FF"
               }
             }) =~ "Application already exists"

      # and checks the new hub on sidebar
      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #FF00FF"/

      assert view
             |> element("#hubs")
             |> render() =~ "/hub/fly-123456789"

      assert view
             |> element("#hubs")
             |> render() =~ "My Foo Hub"
    end

    test "updates fly", %{conn: conn} do
      fly_app_bypass("987654321")
      fly = create_fly("fly-987654321", %{application_id: "987654321"})

      {:ok, view, _html} = live(conn, "/hub/fly-987654321")

      # renders the second step
      assert render(view) =~ "2. Configure your Hub"

      assert render(view) =~
               ~s(<option selected="selected" value="987654321">Foo Bar - 987654321</option>)

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      view
      |> element("#fly-form")
      |> render_submit(%{
        "fly" => %{
          "access_token" => "dummy access token",
          "application_id" => "987654321",
          "hub_name" => "Personal Hub",
          "hub_color" => "#FF00FF"
        }
      })

      # and checks the new hub on sidebar
      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #FF00FF"/

      assert view
             |> element("#hubs")
             |> render() =~ "/hub/fly-987654321"

      assert view
             |> element("#hubs")
             |> render() =~ "Personal Hub"

      refute Hubs.fetch_hub!("fly-987654321") == fly
    end

    test "fails to create existing hub", %{conn: conn} do
      fly = create_fly("fly-foo", %{application_id: "foo"})
      fly_app_bypass("foo")

      {:ok, view, _html} = live(conn, "/hub")

      # renders the second step
      assert view
             |> element("#fly")
             |> render_click() =~ "2. Configure your Hub"

      # triggers the access_token field change
      # and shows the fly's third step
      assert view
             |> element(~s/input[name="fly[access_token]"]/)
             |> render_change(%{"fly" => %{"access_token" => "dummy access token"}}) =~
               ~s(<option value="foo">Foo Bar - foo</option>)

      # triggers the application_id field change
      # and assigns `selected_app` to socket
      view
      |> element(~s/select[name="fly[application_id]"]/)
      |> render_change(%{"fly" => %{"application_id" => "foo"}})

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      view
      |> element("#fly-form")
      |> render_submit(%{
        "fly" => %{
          "access_token" => "dummy access token",
          "application_id" => "foo",
          "hub_name" => "My Foo Hub",
          "hub_color" => "#FF00FF"
        }
      })

      assert render(view) =~ "Application already exists"

      # and checks the hub didn't change on sidebar
      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #{fly.hub_color}"/

      assert view
             |> element("#hubs")
             |> render() =~ "/hub/fly-foo"

      assert view
             |> element("#hubs")
             |> render() =~ fly.hub_name

      assert Hubs.fetch_hub!("fly-foo") == fly
    end
  end

  defp fly_app_bypass(app_id) do
    bypass = Bypass.open()
    Application.put_env(:livebook, :fly_graphql_endpoint, "http://localhost:#{bypass.port}")

    app = %{
      "id" => app_id,
      "organization" => %{
        "id" => "l3soyvjmvtmwtl6l2drnbfuvltipprge",
        "name" => "Foo Bar",
        "type" => "PERSONAL"
      }
    }

    response = %{"data" => %{"apps" => %{"nodes" => [app]}}}

    Bypass.expect(bypass, "POST", "/", fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.resp(200, Jason.encode!(response))
    end)
  end
end
