defmodule LivebookWeb.Hub.New.EnterpriseComponentTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.Hubs

  setup do
    {:ok, url: EnterpriseServer.url(), token: EnterpriseServer.token()}
  end

  describe "enterprise" do
    test "persists new hub", %{conn: conn, url: url, token: token} do
      {:ok, view, _html} = live(conn, Routes.hub_path(conn, :new))

      assert view
             |> element("#enterprise")
             |> render_click() =~ "2. Configure your Hub"

      view
      |> element("#enterprise-form")
      |> render_change(%{
        "enterprise" => %{
          "url" => url,
          "token" => token
        }
      })

      assert view
             |> element("#connect")
             |> render_click() =~ "Add Hub"

      attrs = %{
        "url" => url,
        "token" => token,
        "hub_name" => "Enterprise",
        "hub_color" => "#FF00FF"
      }

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

      assert render(view) =~ "Hub added successfully"

      hubs_html = view |> element("#hubs") |> render()
      assert hubs_html =~ ~s/style="color: #FF00FF"/
      assert hubs_html =~ "/hub/enterprise-bf1587a3-4501-4729-9f53-43679381e28b"
      assert hubs_html =~ "Enterprise"
    end

    test "fails to create existing hub", %{conn: conn, url: url, token: token} do
      hub =
        insert_hub(:enterprise,
          id: "enterprise-bf1587a3-4501-4729-9f53-43679381e28b",
          external_id: "bf1587a3-4501-4729-9f53-43679381e28b",
          url: url,
          token: token
        )

      {:ok, view, _html} = live(conn, Routes.hub_path(conn, :new))

      assert view
             |> element("#enterprise")
             |> render_click() =~ "2. Configure your Hub"

      view
      |> element("#enterprise-form")
      |> render_change(%{
        "enterprise" => %{
          "url" => url,
          "token" => token
        }
      })

      assert view
             |> element("#connect")
             |> render_click() =~ "Add Hub"

      attrs = %{
        "url" => url,
        "token" => token,
        "hub_name" => "Enterprise",
        "hub_color" => "#FFFFFF"
      }

      view
      |> element("#enterprise-form")
      |> render_change(%{"enterprise" => attrs})

      refute view
             |> element("#enterprise-form .invalid-feedback")
             |> has_element?()

      assert view
             |> element("#enterprise-form")
             |> render_submit(%{"enterprise" => attrs}) =~ "already exists"

      hubs_html = view |> element("#hubs") |> render()
      assert hubs_html =~ ~s/style="color: #{hub.hub_color}"/
      assert hubs_html =~ Routes.hub_path(conn, :edit, hub.id)
      assert hubs_html =~ hub.hub_name

      assert Hubs.fetch_hub!(hub.id) == hub
    end
  end
end
