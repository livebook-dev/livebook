defmodule LivebookWeb.Hub.NewLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest

  test "render hub selection cards", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/hub")

    # shows the new options
    assert has_element?(view, "#new-org")
    assert has_element?(view, "#join-org")
  end

  describe "new-org" do
    test "persist a new hub", %{conn: conn, node: node, user: user} do
      name = "New Org Test #{System.unique_integer([:positive])}"
      teams_key = Livebook.Teams.Org.teams_key()

      {:ok, view, _html} = live(conn, ~p"/hub")

      # select the new org option
      assert view
             |> element("#new-org")
             |> render_click() =~ "2. Create your Organization"

      # builds the form data
      org_attrs = %{"new_org" => %{"name" => name, "teams_key" => teams_key}}

      # finds the form and change data
      new_org_form = element(view, "#new-org-form")
      render_change(new_org_form, org_attrs)

      # submits the form 
      render_submit(new_org_form, org_attrs)

      # check if the form has the url to confirm
      link_element = element(view, "#new-org-form a")
      html = render(link_element)
      parsed_html = Floki.parse_document!(html)
      assert [url] = Floki.attribute(parsed_html, "href")
      assert [_port, [org_request_id]] = Regex.scan(~r/(?<=\D|^)\d{1,4}(?=\D|$)/, url)
      id = String.to_integer(org_request_id)

      # confirm org request
      org_request = :erpc.call(node, Hub.Integration, :get_org_request!, [id])
      :erpc.call(node, Hub.Integration, :confirm_org_request, [org_request, user])

      # wait for the c:handle_info/2 cycle
      Process.sleep(1200)

      # shows the new hub form
      assert render(view) =~ "🏭"

      # builds the form data
      hub_attrs = %{"enterprise" => %{"hub_name" => name, "hub_emoji" => "🐈"}}

      # finds the form and change data
      hub_form = element(view, "#hub-form")
      render_change(hub_form, hub_attrs)

      # submits the form and check if the
      # page redirected to edit hub page
      assert {:ok, view, _html} =
               hub_form
               |> render_submit(hub_attrs)
               |> follow_redirect(conn)

      # checks the success flash
      assert render(view) =~ "Hub added successfully"

      # checks the hub in the sidebar
      hubs_html = view |> element("#hubs") |> render()
      assert hubs_html =~ "🐈"
      assert hubs_html =~ name
    end
  end
end
