defmodule LivebookWeb.HubLiveTest do
  use LivebookWeb.ConnCase, async: true

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
        "hub_color" => "#FF00FF"
      }

      view
      |> element("#fly-form")
      |> render_change(%{"fly" => attrs})

      assert view
             |> element("#fly-form")
             |> render()
             |> Floki.parse_document!()
             |> Floki.find(".invalid-feedback") == []

      assert {:ok, view, _html} =
               view
               |> element("#fly-form")
               |> render_submit(%{"fly" => attrs})
               |> follow_redirect(conn)

      assert render(view) =~ "Hub created successfully"

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
      fly = insert_hub(:fly, id: "fly-987654321", application_id: "987654321")

      {:ok, view, _html} = live(conn, "/hub/fly-987654321")

      assert render(view) =~ "2. Configure your Hub"

      assert render(view) =~
               ~s(<option selected="selected" value="987654321">Foo Bar - 987654321</option>)

      attrs = %{
        "access_token" => "dummy access token",
        "application_id" => "987654321",
        "hub_name" => "Personal Hub",
        "hub_color" => "#FF00FF"
      }

      view
      |> element("#fly-form")
      |> render_change(%{"fly" => attrs})

      assert view
             |> element("#fly-form")
             |> render()
             |> Floki.parse_document!()
             |> Floki.find(".invalid-feedback") == []

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
             |> render() =~ "/hub/fly-987654321"

      assert view
             |> element("#hubs")
             |> render() =~ "Personal Hub"

      refute Hubs.fetch_hub!("fly-987654321") == fly
    end

    test "fails to create existing hub", %{conn: conn} do
      fly = insert_hub(:fly, id: "fly-foo", application_id: "foo")
      fly_app_bypass("foo")

      {:ok, view, _html} = live(conn, "/hub")

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
        "hub_color" => "#FF00FF"
      }

      view
      |> element("#fly-form")
      |> render_change(%{"fly" => attrs})

      assert view
             |> element("#fly-form")
             |> render()
             |> Floki.parse_document!()
             |> Floki.find(".invalid-feedback") == []

      assert view
             |> element("#fly-form")
             |> render_submit(%{"fly" => attrs}) =~ "already exists"

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
