defmodule LivebookWeb.Hub.New.EnterpriseComponentTest do
  use Livebook.EnterpriseIntegrationCase, async: true

  import Phoenix.LiveViewTest

  alias Livebook.Hubs

  describe "enterprise" do
    test "persists new hub", %{conn: conn, url: url, token: token, user: user} do
      Livebook.Hubs.delete_hub("enterprise-#{user.id}")

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

      view
      |> element("#connect")
      |> render_click()

      assert render(view) =~ to_string(user.id)

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

      result =
        view
        |> element("#enterprise-form")
        |> render_submit(%{"enterprise" => attrs})

      assert {:ok, view, _html} = follow_redirect(result, conn)

      assert render(view) =~ "Hub added successfully"

      hubs_html = view |> element("#hubs") |> render()
      assert hubs_html =~ ~s/style="color: #FF00FF"/
      assert hubs_html =~ "/hub/enterprise-#{user.id}"
      assert hubs_html =~ "Enterprise"
    end

    test "fails with invalid token", %{conn: conn, url: url} do
      {:ok, view, _html} = live(conn, Routes.hub_path(conn, :new))
      token = "foo bar baz"

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

      view
      |> element("#connect")
      |> render_click()

      assert render(view) =~ "the given token is invalid"
      refute render(view) =~ "enterprise[hub_name]"
    end

    test "fails to create existing hub", %{conn: conn, url: url, token: token, user: user} do
      hub =
        insert_hub(:enterprise,
          id: "enterprise-#{user.id}",
          external_id: user.id,
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

      view
      |> element("#connect")
      |> render_click()

      assert render(view) =~ to_string(user.id)

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
