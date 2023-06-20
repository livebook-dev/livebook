defmodule Livebook.HubHelpers do
  @moduledoc false

  import ExUnit.Assertions
  import Livebook.Factory
  import Phoenix.LiveViewTest

  def create_team_hub(user, node) do
    hub = build_team_hub(user, node)
    Livebook.Hubs.save_hub(hub)
  end

  def build_team_headers(user, node) do
    hub = build_team_hub(user, node)

    headers = [
      {"x-user", to_string(hub.user_id)},
      {"x-org", to_string(hub.org_id)},
      {"x-org-key", to_string(hub.org_key_id)},
      {"x-session-token", hub.session_token}
    ]

    {hub, headers}
  end

  defp build_team_hub(user, node) do
    teams_org = build(:org)
    teams_key = teams_org.teams_key
    key_hash = Livebook.Teams.Org.key_hash(teams_org)

    org = erpc_call(node, :create_org, [])
    org_key = erpc_call(node, :create_org_key, [[org: org, key_hash: key_hash]])
    org_key_pair = erpc_call(node, :create_org_key_pair, [[org: org]])
    token = erpc_call(node, :associate_user_with_org, [user, org])

    build(:team,
      id: "team-#{org.name}",
      hub_name: org.name,
      user_id: user.id,
      org_id: org.id,
      org_key_id: org_key.id,
      org_public_key: org_key_pair.public_key,
      session_token: token,
      teams_key: teams_key
    )
  end

  def assert_sidebar_hub(view, id, name, emoji \\ "🐈") do
    hub = element(view, hub_element_id(id))
    hub_html = render(hub)

    assert hub_html =~ emoji
    assert hub_html =~ "/hub/#{id}"
    assert hub_html =~ name
  end

  def refute_sidebar_hub(view, id) do
    refute has_element?(view, hub_element_id(id))
  end

  defp hub_element_id(id), do: "#hubs #hub-#{id}"

  defp erpc_call(node, fun, args) do
    :erpc.call(node, Hub.Integration, fun, args)
  end
end
