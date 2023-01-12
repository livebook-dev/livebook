defmodule LivebookWeb.Hub.New.EnterpriseComponentTest do
  use Livebook.EnterpriseIntegrationCase, async: true
  @moduletag :capture_log

  import Phoenix.LiveViewTest

  alias Livebook.Hubs

  describe "enterprise" do
    test "persists new hub", %{conn: conn, url: url, token: token} do
      node = EnterpriseServer.get_node()
      id = :erpc.call(node, Enterprise.Integration, :fetch_env!, [])
      Livebook.Hubs.delete_hub("enterprise-#{id}")

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

      assert render(view) =~ to_string(id)

      attrs = %{
        "url" => url,
        "token" => token,
        "hub_name" => "Enterprise",
        "hub_emoji" => "🐈"
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
      assert hubs_html =~ "🐈"
      assert hubs_html =~ "/hub/enterprise-#{id}"
      assert hubs_html =~ "Enterprise"
    end

    test "fails with invalid token", %{test: name, conn: conn} do
      start_new_instance(name)

      url = EnterpriseServer.url(name)

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
    after
      stop_new_instance(name)
    end

    test "fails to create existing hub", %{test: name, conn: conn} do
      start_new_instance(name)

      node = EnterpriseServer.get_node(name)
      url = EnterpriseServer.url(name)
      token = EnterpriseServer.token(name)

      id = :erpc.call(node, Enterprise.Integration, :fetch_env!, [])
      user = :erpc.call(node, Enterprise.Integration, :create_user, [])

      another_token =
        :erpc.call(node, Enterprise.Integration, :generate_user_session_token!, [user])

      hub =
        insert_hub(:enterprise,
          id: "enterprise-#{id}",
          external_id: id,
          url: url,
          token: another_token
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

      assert render(view) =~ to_string(id)

      attrs = %{
        "url" => url,
        "token" => token,
        "hub_name" => "Enterprise",
        "hub_emoji" => "🐈"
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
      assert hubs_html =~ hub.hub_emoji
      assert hubs_html =~ Routes.hub_path(conn, :edit, hub.id)
      assert hubs_html =~ hub.hub_name

      assert Hubs.fetch_hub!(hub.id) == hub
    after
      stop_new_instance(name)
    end
  end

  defp start_new_instance(name) do
    suffix = Ecto.UUID.generate() |> :erlang.phash2() |> to_string()
    app_port = Enum.random(1000..9000) |> to_string()

    {:ok, _} =
      EnterpriseServer.start(name,
        env: %{"ENTERPRISE_DB_SUFFIX" => suffix},
        app_port: app_port
      )
  end

  defp stop_new_instance(name) do
    EnterpriseServer.disconnect(name)
    EnterpriseServer.drop_database(name)
  end
end
