defmodule LivebookWeb.HubLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.Hubs
  alias Livebook.Hubs.Hub

  setup do
    on_exit(fn ->
      for %{id: hub_id} <- Hubs.fetch_hubs() do
        Livebook.Storage.current().delete(:hub, hub_id)
      end
    end)

    :ok
  end

  test "render hub selection cards", %{conn: conn} do
    {:ok, _view, html} = live(conn, "/hub")

    assert html =~ "Fly"
    assert html =~ "Livebook Enterprise"
  end

  describe "fly" do
    test "renders the second step", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/hub")

      assert view
             |> element("#fly")
             |> render_click() =~ "Deploy notebooks to your Fly account."
    end

    test "renders filled form with existing hub", %{conn: conn} do
      hub = %Hub{
        id: "l3soyvjmvtmwtl6l2drnbfuvltipprge",
        type: "fly",
        name: "My Foo",
        label: "Foo Bar",
        token: "foo",
        color: "#FF00FF"
      }

      Hubs.save_hub(hub)

      {:ok, _view, html} = live(conn, "/hub/" <> hub.id)

      assert html =~ hub.name
      assert html =~ hub.color
    end

    test "persists hub", %{conn: conn} do
      bypass = Bypass.open()
      Application.put_env(:livebook, :fly_io_graphql_endpoint, "http://localhost:#{bypass.port}")

      {:ok, view, _html} = live(conn, "/hub")

      organization = %{
        "id" => "123456789",
        "slug" => "personal",
        "name" => "Foo Bar",
        "type" => "PERSONAL"
      }

      response = %{"data" => %{"organizations" => %{"nodes" => [organization]}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      # renders the second step
      assert view
             |> element("#fly")
             |> render_click()

      # triggers the access_token field change
      # and shows the fly's third step
      assert view
             |> element(~s/input[name="fly[token]"]/)
             |> render_change(%{"fly" => %{"token" => "dummy token"}}) =~
               ~s(<option value="#{organization["id"]}">Foo Bar</option>)

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      refute view
             |> element("#fly-form")
             |> render_submit(%{
               "fly" => %{
                 "token" => "dummy token",
                 "organization" => organization["id"],
                 "name" => "My Foo Hub",
                 "hex_color" => "#FF00FF"
               }
             }) =~ "Hub already exists"

      # and checks the new hub on sidebar
      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #FF00FF"/

      assert view
             |> element("#hubs")
             |> render() =~ "/hub/" <> organization["id"]

      assert view
             |> element("#hubs")
             |> render() =~ "My Foo Hub"
    end

    test "updates hub", %{conn: conn} do
      hub = %Hub{
        id: "l3soyvjmvtmwtl6l2drnbfuvltipprge",
        type: "fly",
        name: "Foo",
        label: "Foo Bar",
        token: "foo",
        color: "#FF00FF"
      }

      Hubs.save_hub(hub)

      {:ok, view, _html} = live(conn, "/hub/" <> hub.id)

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      assert view
             |> element("#fly-form")
             |> render_submit(%{
               "fly" => %{
                 "token" => "dummy token",
                 "organization" => hub.id,
                 "name" => "My Foo Hub",
                 "hex_color" => "#FF00FF"
               }
             })

      # and checks the new hub on sidebar
      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #FF00FF"/

      assert view
             |> element("#hubs")
             |> render() =~ "/hub/" <> hub.id

      assert view
             |> element("#hubs")
             |> render() =~ "My Foo Hub"

      refute hub == Hubs.hub_by_id!(hub.id)
    end

    test "fails to create existing hub", %{conn: conn} do
      bypass = Bypass.open()
      Application.put_env(:livebook, :fly_io_graphql_endpoint, "http://localhost:#{bypass.port}")

      hub = %Hub{
        id: "l3soyvjmvtmwtl6l2drnbfuvltipprge",
        type: "fly",
        name: "Foo",
        label: "Foo Bar",
        token: "dummy token",
        color: "#0000FF"
      }

      Hubs.save_hub(hub)

      {:ok, view, _html} = live(conn, "/hub")

      organization = %{
        "id" => "l3soyvjmvtmwtl6l2drnbfuvltipprge",
        "slug" => "personal",
        "name" => "Foo Bar",
        "type" => "PERSONAL"
      }

      response = %{"data" => %{"organizations" => %{"nodes" => [organization]}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      # renders the second step
      assert view
             |> element("#fly")
             |> render_click()

      # triggers the access_token field change
      # and shows the fly's third step
      assert view
             |> element(~s/input[name="fly[token]"]/)
             |> render_change(%{"fly" => %{"token" => "dummy token"}}) =~
               ~s(<option value="#{organization["id"]}">Foo Bar</option>)

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      assert view
             |> element("#fly-form")
             |> render_submit(%{
               "fly" => %{
                 "token" => "dummy token",
                 "organization" => organization["id"],
                 "name" => "My Foo Hub",
                 "hex_color" => "#FF00FF"
               }
             }) =~ "Hub already exists"

      # and checks the hub didn't change on sidebar
      assert view
             |> element("#hubs")
             |> render() =~ ~s/style="color: #{hub.color}"/

      assert view
             |> element("#hubs")
             |> render() =~ "/hub/" <> hub.id

      assert view
             |> element("#hubs")
             |> render() =~ hub.name
    end
  end
end
