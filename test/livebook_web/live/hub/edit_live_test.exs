defmodule LivebookWeb.Hub.EditLiveTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Livebook.Hubs

  setup do
    on_exit(&Hubs.clean_hubs/0)
    :ok
  end

  describe "fly" do
    test "updates fly", %{conn: conn} do
      hub = insert_hub(:fly, id: "fly-987654321", application_id: "987654321")
      fly_bypass(hub.application_id)

      {:ok, view, html} = live(conn, Routes.hub_path(conn, :edit, hub.id))

      assert html =~ "See app on Fly"
      assert html =~ "https://#{hub.application_id}.fly.dev"

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
             |> render() =~ "/hub/fly-987654321"

      assert view
             |> element("#hubs")
             |> render() =~ "Personal Hub"

      refute Hubs.fetch_hub!(hub.id) == hub
    end
  end

  defp fly_bypass(app_id) do
    bypass = Bypass.open()
    Application.put_env(:livebook, :fly_graphql_endpoint, "http://localhost:#{bypass.port}")

    Bypass.expect(bypass, "POST", "/", fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)

      response =
        case Jason.decode!(body) do
          %{"variables" => %{"appId" => ^app_id}} -> fetch_app_response(app_id)
          %{"variables" => %{}} -> fetch_apps_response(app_id)
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
      "status" => "running"
    }

    %{"data" => %{"app" => app}}
  end
end
