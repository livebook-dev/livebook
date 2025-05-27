defmodule Livebook.TeamsIntegrationHelper do
  alias Livebook.{Factory, Hubs, Teams, TeamsRPC}

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

  def livebook_teams_auth(%{conn: conn, node: node, team: team, teams_for: :agent} = context) do
    ExUnit.Callbacks.start_supervised!(
      {Livebook.ZTA.LivebookTeams, name: LivebookWeb.ZTA, identity_key: team.id}
    )

    {conn, code} = authenticate_user_on_teams(conn, node, team)
    Map.merge(context, %{conn: conn, code: code})
  end

  @doc false
  def create_user_hub(node) do
    context = new_user_hub(node)
    id = context.team.id
    Hubs.save_hub(context.team)
    pid = Hubs.TeamClient.get_pid(id)

    assert Process.alive?(pid)
    assert Hubs.hub_exists?(id)
    assert_receive {:hub_connected, ^id}, 3_000
    assert_receive {:client_connected, ^id}, 3_000

    ExUnit.Callbacks.on_exit(fn -> Hubs.delete_hub(id) end)
    context
  end

  @doc false
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

  @doc false
  def create_agent_hub(node, opts \\ []) do
    context = new_agent_hub(node, opts)
    id = context.team.id
    Hubs.save_hub(context.team)

    deployment_group_id = to_string(context.deployment_group.id)
    org_id = to_string(context.org.id)
    pid = Hubs.TeamClient.get_pid(id)

    assert Process.alive?(pid)
    assert Hubs.hub_exists?(id)
    assert_receive {:hub_connected, ^id}, 3_000
    assert_receive {:client_connected, ^id}, 3_000

    assert_receive {:agent_joined,
                    %{hub_id: ^id, deployment_group_id: ^deployment_group_id, org_id: ^org_id} =
                      agent},
                   3_000

    ExUnit.Callbacks.on_exit(fn -> Hubs.delete_hub(id) end)
    Map.put_new(%{context | team: Hubs.fetch_hub!(id)}, :agent, agent)
  end

  @doc false
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

  @doc false
  def authenticate_user_on_teams(conn, node, team) do
    response =
      conn
      |> LivebookWeb.ConnCase.with_authorization(team.id)
      |> get("/")
      |> html_response(200)

    [_, location] = Regex.run(~r/URL\("(.*?)"\)/, response)
    uri = URI.parse(location)
    %{"token" => token} = URI.decode_query(uri.query)

    %{code: code} = Livebook.TeamsRPC.allow_auth_request(node, token)

    session =
      conn
      |> LivebookWeb.ConnCase.with_authorization(team.id)
      |> get("/", %{teams_identity: "", code: code})
      |> Plug.Conn.get_session()

    conn = Plug.Test.init_test_session(conn, session)
    authenticated_conn = get(conn, "/")
    assigns = Map.take(authenticated_conn.assigns, [:current_user])

    {%Plug.Conn{conn | assigns: Map.merge(conn.assigns, assigns)}, code}
  end

  # Private

  defp generate_key_hash(teams_key \\ Teams.Org.teams_key()) do
    {teams_key, Teams.Org.key_hash(%Teams.Org{teams_key: teams_key})}
  end
end
