defmodule Livebook.TeamsIntegrationHelper do
  alias Livebook.{Factory, Hubs, Teams, TeamsRPC, ZTA}

  import ExUnit.Assertions
  import Phoenix.ConnTest

  @endpoint LivebookWeb.Endpoint

  def teams(context) do
    case {context[:teams_for], context[:teams_persisted]} do
      {:user, false} -> Map.merge(context, new_user_hub(context.node))
      {:user, _} -> Map.merge(context, create_user_hub(context.node))
      {:agent, false} -> Map.merge(context, new_agent_hub(context.node))
      {:agent, _} -> Map.merge(context, create_agent_hub(context.node))
      _otherwise -> context
    end
  end

  def livebook_teams_auth(%{conn: conn, node: node, team: team} = context) do
    ZTA.LivebookTeams.start_link(name: context.test, identity_key: team.id)
    {conn, code} = authenticate_user_on_teams(context.test, conn, node, team)

    Map.merge(context, %{conn: conn, code: code})
  end

  defp create_user_hub(node) do
    context = new_user_hub(node)
    Hubs.save_hub(context.team)

    ExUnit.Callbacks.on_exit(fn ->
      Hubs.delete_hub(context.team.id)
    end)

    wait_until_client_start(context)
  end

  defp new_user_hub(node) do
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
        hub_emoji: "💡",
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
      session_token: token,
      team: team
    }
  end

  defp create_agent_hub(node, opts \\ []) do
    context = new_agent_hub(node, opts)
    Hubs.save_hub(context.team)

    ExUnit.Callbacks.on_exit(fn ->
      Hubs.delete_hub(context.team.id)
    end)

    wait_until_agent_start(context)
  end

  defp new_agent_hub(node, opts \\ []) do
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
        hub_emoji: "💡",
        user_id: nil,
        org_id: org.id,
        org_key_id: org_key.id,
        org_public_key: org_key_pair.public_key,
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

  defp authenticate_user_on_teams(name, conn, node, team) do
    # Create a fresh connection to avoid session contamination
    fresh_conn = Phoenix.ConnTest.build_conn()

    response =
      fresh_conn
      |> LivebookWeb.ConnCase.with_authorization(team.id, name)
      |> get("/")
      |> html_response(200)

    [_, location] = Regex.run(~r/URL\("(.*?)"\)/, response)
    uri = URI.parse(location)
    %{"token" => token} = URI.decode_query(uri.query)

    %{code: code} = Livebook.TeamsRPC.allow_auth_request(node, token)

    session =
      fresh_conn
      |> LivebookWeb.ConnCase.with_authorization(team.id, name)
      |> get("/", %{teams_identity: "", code: code})
      |> Plug.Conn.get_session()

    # Initialize the original conn with the new session data
    authenticated_conn = Plug.Test.init_test_session(conn, session)
    final_conn = get(authenticated_conn, "/")
    assigns = Map.take(final_conn.assigns, [:current_user])

    {%Plug.Conn{authenticated_conn | assigns: Map.merge(authenticated_conn.assigns, assigns)},
     code}
  end

  def change_to_agent_session(%{node: node, teams_for: :user} = context) do
    pid = Hubs.TeamClient.get_pid(context.team.id)
    Hubs.TeamClient.stop(context.team.id)
    refute Process.alive?(pid)

    agent_key = context[:agent_key] || TeamsRPC.create_agent_key(node, org: context.org)

    deployment_group =
      context[:deployment_group] ||
        TeamsRPC.create_deployment_group(node, mode: :online, org: context.org)

    team = %{context.team | user_id: nil, session_token: agent_key.key}

    Hubs.save_hub(team)

    %{context | teams_for: :agent}
    |> Map.put_new(:agent_key, agent_key)
    |> Map.put_new(:deployment_group, deployment_group)
    |> wait_until_agent_start()
  end

  def change_to_user_session(%{node: node, org: org, teams_for: :agent} = context) do
    pid = Hubs.TeamClient.get_pid(context.team.id)
    Hubs.TeamClient.stop(context.team.id)
    refute Process.alive?(pid)

    user = context[:user] || TeamsRPC.create_user(node)
    session_token = context[:session_token] || TeamsRPC.associate_user_with_org(node, user, org)
    team = %{context.team | user_id: user.id, session_token: session_token}

    Hubs.save_hub(team)
    wait_until_client_start(%{context | team: team, teams_for: :user})
  end

  # Private

  defp wait_until_client_start(context) do
    id = context.team.id
    pid = Hubs.TeamClient.get_pid(id)

    assert Process.alive?(pid)
    assert Hubs.hub_exists?(id)

    assert_receive {:hub_connected, ^id}, 3_000
    assert_receive {:client_connected, ^id}, 3_000

    context
  end

  defp wait_until_agent_start(context) do
    context = wait_until_client_start(context)
    id = context.team.id
    deployment_group_id = to_string(context.deployment_group.id)
    org_id = to_string(context.org.id)

    assert_receive {:agent_joined,
                    %{hub_id: ^id, deployment_group_id: ^deployment_group_id, org_id: ^org_id} =
                      agent},
                   3_000

    Map.put_new(context, :agent, agent)
  end

  defp generate_key_hash(teams_key \\ Teams.Org.teams_key()) do
    {teams_key, Teams.Org.key_hash(%Teams.Org{teams_key: teams_key})}
  end
end
