defmodule Livebook.Integration.TeamsTest do
  alias Livebook.{Factory, Hubs, Teams, TeamsRPC}

  import ExUnit.Assertions
  # import Phoenix.LiveViewTest

  def workspace(context) do
    case {context[:workspace_for], context[:persisted]} do
      {:user, false} -> Map.merge(context, new_user_hub(context.node))
      {:user, _} -> Map.merge(context, create_user_hub(context.node))
      {:agent, false} -> Map.merge(context, new_agent_hub(context.node))
      {:agent, _} -> Map.merge(context, create_agent_hub(context.node))
      _otherwise -> context
    end
  end

  def create_user_hub(node) do
    context = new_user_hub(node)
    id = context.team.id
    Hubs.save_hub(context.team)

    assert_receive {:hub_connected, ^id}, 3_000
    assert_receive {:client_connected, ^id}, 3_000

    ExUnit.Callbacks.on_exit(fn -> Hubs.delete_hub(id) end)

    context
  end

  def new_user_hub(node) do
    {teams_key, key_hash} = generate_key_hash()

    org = TeamsRPC.create_org(node)
    user = TeamsRPC.create_user(node)
    org_key = TeamsRPC.create_org_key(node, org: org, key_hash: key_hash)
    org_key_pair = TeamsRPC.create_org_key_pair(node, org: org)
    token = TeamsRPC.associate_user_with_org(node, user, org)
    TeamsRPC.create_billing_subscription(node, org)

    team =
      Factory.build(:team,
        id: "team-#{org.name}",
        hub_name: org.name,
        user_id: user.id,
        org_id: org.id,
        org_key_id: org_key.id,
        org_public_key: org_key_pair.public_key,
        session_token: token,
        teams_key: teams_key,
        billing_status: %{disabled: false, type: nil}
      )

    %{
      org: org,
      user: user,
      org_key: org_key,
      org_key_pair: org_key_pair,
      team: team
    }
  end

  def create_agent_hub(node, opts \\ []) do
    context = new_agent_hub(node, opts)
    id = context.team.id
    Hubs.save_hub(context.team)

    assert_receive {:hub_connected, ^id}, 3_000
    assert_receive {:client_connected, ^id}, 3_000

    ExUnit.Callbacks.on_exit(fn -> Hubs.delete_hub(id) end)

    context
  end

  def new_agent_hub(node, opts \\ []) do
    {teams_key, key_hash} = generate_key_hash()

    org = TeamsRPC.create_org(node)
    org_key = TeamsRPC.create_org_key(node, org: org, key_hash: key_hash)
    org_key_pair = TeamsRPC.create_org_key_pair(node, org: org)

    attrs =
      opts
      |> Keyword.get(:deployment_group, [])
      |> Keyword.merge(
        name: "sleepy-cat-#{Ecto.UUID.generate()}",
        mode: :online,
        org: org
      )

    deployment_group = TeamsRPC.create_deployment_group(node, attrs)
    agent_key = TeamsRPC.create_agent_key(node, deployment_group: deployment_group)

    TeamsRPC.create_billing_subscription(node, org)

    team =
      Factory.build(:team,
        id: "team-#{org.name}",
        hub_name: org.name,
        user_id: nil,
        org_id: org.id,
        org_key_id: org_key.id,
        org_public_key: nil,
        session_token: agent_key.key,
        teams_key: teams_key
      )

    %{
      agent_key: agent_key,
      deployment_group: deployment_group,
      org: org,
      org_key: org_key,
      org_key_pair: org_key_pair,
      team: team
    }
  end

  def get_headers(%Hubs.Team{user_id: nil} = team) do
    [
      {"x-lb-version", Livebook.Config.app_version()},
      {"x-org", to_string(team.org_id)},
      {"x-org-key", to_string(team.org_key_id)},
      {"x-agent-name", Livebook.Config.agent_name()},
      {"x-agent-key", team.session_token}
    ]
  end

  def get_headers(%Hubs.Team{} = team) do
    [
      {"x-lb-version", Livebook.Config.app_version()},
      {"x-user", to_string(team.user_id)},
      {"x-org", to_string(team.org_id)},
      {"x-org-key", to_string(team.org_key_id)},
      {"x-session-token", team.session_token}
    ]
  end

  # Private

  defp generate_key_hash(teams_key \\ Teams.Org.teams_key()) do
    {teams_key, Teams.Org.key_hash(%Teams.Org{teams_key: teams_key})}
  end
end
