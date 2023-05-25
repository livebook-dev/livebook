defmodule LivebookWeb.Hub.NewLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Teams.Org

  import Phoenix.LiveViewTest
  @check_completion_data_interval 5000

  test "render hub selection cards", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/hub")

    # shows the new options
    assert has_element?(view, "#new-org")
    assert has_element?(view, "#join-org")
  end

  describe "new-org" do
    test "persist a new hub", %{conn: conn, node: node, user: user} do
      name = "new-org-test"
      teams_key = Livebook.Teams.Org.teams_key()
      key_hash = Org.key_hash(build(:org, teams_key: teams_key))
      path = ~p"/hub/team-#{name}"

      {:ok, view, _html} = live(conn, ~p"/hub")

      # select the new org option
      view
      |> element("#new-org")
      |> render_click()

      # builds the form data
      attrs = %{"org" => %{"name" => name, "teams_key" => teams_key, "emoji" => "🐈"}}

      # finds the form and change data
      form = element(view, "#new-org-form")
      render_change(form, attrs)

      # submits the form
      render_submit(form, attrs)

      # gets the org request by name and key hash
      org_request =
        :erpc.call(node, Hub.Integration, :get_org_request_by!, [
          [name: name, key_hash: key_hash]
        ])

      # check if the form has the url to confirm
      link_element = element(view, "#new-org-form a")
      assert render(link_element) =~ "/org-request/#{org_request.id}/confirm"

      # force org request confirmation
      :erpc.call(node, Hub.Integration, :confirm_org_request, [org_request, user])

      # wait for the c:handle_info/2 cycle
      # check if the page redirected to edit hub page
      # and check the flash message
      %{"success" => "Hub added successfully"} =
        assert_redirect(view, path, @check_completion_data_interval)

      # checks if the hub is in the sidebar
      {:ok, view, _html} = live(conn, path)
      hubs_html = view |> element("#hubs") |> render()
      assert hubs_html =~ "🐈"
      assert hubs_html =~ path
      assert hubs_html =~ name
    end
  end

  describe "join-org" do
    test "persist a new hub", %{conn: conn, node: node, user: user} do
      name = "join-org-test"
      teams_key = Livebook.Teams.Org.teams_key()
      key_hash = Org.key_hash(build(:org, teams_key: teams_key))
      path = ~p"/hub/team-#{name}"

      {:ok, view, _html} = live(conn, ~p"/hub")

      # previously create the org and associate user with org
      org = :erpc.call(node, Hub.Integration, :create_org, [[name: name]])
      :erpc.call(node, Hub.Integration, :create_org_key, [[org: org, key_hash: key_hash]])
      :erpc.call(node, Hub.Integration, :create_user_org, [[org: org, user: user]])

      # select the new org option
      view
      |> element("#join-org")
      |> render_click()

      # builds the form data
      attrs = %{"org" => %{"name" => name, "teams_key" => teams_key, "emoji" => "🐈"}}

      # finds the form and change data
      form = element(view, "#join-org-form")
      render_change(form, attrs)

      # submits the form
      render_submit(form, attrs)

      # gets the org request by name and key hash
      org_request =
        :erpc.call(node, Hub.Integration, :get_org_request_by!, [
          [name: name, key_hash: key_hash]
        ])

      # check if the form has the url to confirm
      link_element = element(view, "#join-org-form a")
      assert render(link_element) =~ "/org-request/#{org_request.id}/confirm"

      # force org request confirmation
      :erpc.call(node, Hub.Integration, :confirm_org_request, [org_request, user])

      # wait for the c:handle_info/2 cycle
      # check if the page redirected to edit hub page
      # and check the flash message
      %{"success" => "Hub added successfully"} =
        assert_redirect(view, path, @check_completion_data_interval)

      # checks if the hub is in the sidebar
      {:ok, view, _html} = live(conn, path)
      hubs_html = view |> element("#hubs") |> render()
      assert hubs_html =~ "🐈"
      assert hubs_html =~ path
      assert hubs_html =~ name
    end
  end
end
