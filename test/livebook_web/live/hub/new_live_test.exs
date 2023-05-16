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
      path = ~p"/hub/enterprise-#{name}"

      # select the new org option
      assert view
             |> element("#new-org")
             |> render_click() =~ "2. Create your Organization"

      # builds the form data
      org_attrs = %{"new_org" => %{"name" => name, "teams_key" => teams_key, "emoji" => "🐈"}}

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

      # force org request confirmation
      org_request = :erpc.call(node, Hub.Integration, :get_org_request!, [id])
      :erpc.call(node, Hub.Integration, :confirm_org_request, [org_request, user])

      # wait for the c:handle_info/2 cycle
      # check if the page redirected to edit hub page
      # and check the flash message
      %{"success" => "Hub added successfully"} = assert_redirect(view, path, 1200)

      # checks if the hub is in the sidebar
      {:ok, view, _html} = live(conn, path)
      hubs_html = view |> element("#hubs") |> render()
      assert hubs_html =~ "🐈"
      assert hubs_html =~ path
      assert hubs_html =~ name
    end
  end
end
