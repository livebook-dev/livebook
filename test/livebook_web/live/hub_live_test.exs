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
      fly_organization_bypass("123456789")

      {:ok, view, _html} = live(conn, "/hub")

      # renders the second step
      assert view
             |> element("#fly")
             |> render_click() =~ "2. Configure your Hub"

      # triggers the access_token field change
      # and shows the fly's third step
      assert view
             |> element(~s/input[name="fly[token]"]/)
             |> render_change(%{"fly" => %{"token" => "dummy token"}}) =~
               ~s(<option value="123456789">Foo Bar Baz</option>)

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      refute view
             |> element("#fly-form")
             |> render_submit(%{
               "fly" => %{
                 "token" => "dummy token",
                 "organization" => "123456789",
                 "name" => "My Foo Hub",
                 "color" => "#FF00FF"
               }
             }) =~ "Organization already exists"

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
      fly = create_fly("987654321")

      {:ok, view, _html} = live(conn, "/hub/fly-987654321")

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      view
      |> element("#fly-form")
      |> render_submit(%{
        "fly" => %{
          "token" => "dummy token",
          "organization" => "987654321",
          "name" => "Personal Hub",
          "color" => "#FFFFFF"
        }
      })

      # and checks the new hub on sidebar
      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #FFFFFF"/

      assert view
             |> element("#hubs")
             |> render() =~ "/hub/fly-987654321"

      assert view
             |> element("#hubs")
             |> render() =~ "Personal Hub"

      refute Hubs.fetch_fly!("fly-987654321") == fly
    end

    test "fails to create existing hub", %{conn: conn} do
      fly = create_fly("foo")
      fly_organization_bypass("foo")

      {:ok, view, _html} = live(conn, "/hub")

      # renders the second step
      assert view
             |> element("#fly")
             |> render_click() =~ "2. Configure your Hub"

      # triggers the access_token field change
      # and shows the fly's third step
      assert view
             |> element(~s/input[name="fly[token]"]/)
             |> render_change(%{"fly" => %{"token" => "dummy token"}}) =~
               ~s(<option value="foo">Foo Bar Baz</option>)

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      view
      |> element("#fly-form")
      |> render_submit(%{
        "fly" => %{
          "token" => "dummy token",
          "organization" => "foo",
          "name" => "My Foo Hub",
          "color" => "#FF00FF"
        }
      })

      assert render(view) =~ "Organization already exists"

      # and checks the hub didn't change on sidebar
      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #{fly.color}"/

      assert view
             |> element("#hubs")
             |> render() =~ "/hub/fly-foo"

      assert view
             |> element("#hubs")
             |> render() =~ fly.name

      assert Hubs.fetch_fly!("fly-foo") == fly
    end
  end

  defp fly_organization_bypass(org_id) do
    bypass = Bypass.open()
    Application.put_env(:livebook, :fly_io_graphql_endpoint, "http://localhost:#{bypass.port}")

    organization = %{
      "id" => org_id,
      "slug" => "personal",
      "name" => "Foo Bar Baz",
      "type" => "PERSONAL"
    }

    response = %{"data" => %{"organizations" => %{"nodes" => [organization]}}}

    Bypass.expect_once(bypass, "POST", "/", fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.resp(200, Jason.encode!(response))
    end)
  end
end
