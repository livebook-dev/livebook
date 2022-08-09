defmodule LivebookWeb.HubLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  setup tags do
    if hub = tags[:hub] do
      Application.put_env(:livebook, :feature_flags, hub: hub)
    end

    :ok
  end

  test "render hub selection cards", %{conn: conn} do
    {:ok, _view, html} = live(conn, "/hub")

    assert html =~ "Fly"
    assert html =~ "Livebook Enterprise"
  end

  describe "fly" do
    test "render the second step", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/hub")

      assert view
             |> element("#fly")
             |> render_hook("select_hub_service", %{"value" => "fly"}) =~
               "Connect to your Hub with the following form"
    end

    test "persists hub", %{conn: conn} do
      bypass = Bypass.open()
      Application.put_env(:livebook, :fly_io_graphql_endpoint, "http://localhost:#{bypass.port}")

      {:ok, view, _html} = live(conn, "/hub")

      app = %{
        "id" => "my-foo-application",
        "name" => "my-foo-application",
        "deployed" => true,
        "hostname" => "https://my-foo-application.fly.dev",
        "organization" => %{
          "id" => "l3soyvjmvtmwtl6l2drnbfuvltipprge",
          "slug" => "personal",
          "name" => "Foo Bar",
          "type" => "PERSONAL"
        },
        "latestImageDetails" => %{
          "registry" => "registry-1.docker.io",
          "repository" => "livebook/livebook",
          "tag" => "0.6.3"
        },
        "state" => "DEPLOYED"
      }

      response = %{"data" => %{"apps" => %{"nodes" => [app]}}}

      Bypass.expect_once(bypass, "POST", "/", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, Jason.encode!(response))
      end)

      # renders the second step
      assert view
             |> element("#fly")
             |> render_hook("select_hub_service", %{"value" => "fly"})

      # triggers the access_token field change
      # and shows the fly's third step
      assert view
             |> element(~s/input[name="fly[token]"]/)
             |> render_change(%{"fly" => %{"token" => "dummy token"}}) =~
               ~s(<option value="my-foo-application">Foo Bar - my-foo-application</option>)

      # sends the save_hub event to backend
      # and checks the new hub on sidebar
      assert view
             |> element("#fly-form")
             |> render_submit(%{
               "fly" => %{
                 "token" => "dummy token",
                 "application" => "my-foo-application",
                 "name" => "My Foo Hub",
                 "hex_color" => "#FF00FF"
               }
             })

      # and checks the new hub on sidebar
      {:ok, _view, html} = live(conn, "/hub")

      assert html
             |> Floki.parse_document!()
             |> Floki.find(".sidebar--hub")
             |> Floki.find(".ri-checkbox-blank-circle-fill")
             |> Floki.attribute("style") == ["color: #FF00FF"]

      assert html
             |> Floki.parse_document!()
             |> Floki.find(".sidebar--hub")
             |> Floki.find(".ml-1.text-sm.font-medium")
             |> Floki.text() =~ "My Foo Hub"
    end
  end

  describe "enterprise" do
    test "doesn't render the second step", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/hub")

      assert_raise ArgumentError,
                   ~s/element selected by "#enterprise" does not have phx-hook attribute/,
                   fn ->
                     view
                     |> element("#enterprise")
                     |> render_hook("select_hub_service", %{"value" => "enterprise"})
                   end
    end
  end
end
