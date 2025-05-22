defmodule Livebook.TeamsIntegrationCase do
  use ExUnit.CaseTemplate

  import Phoenix.ConnTest
  alias Livebook.TeamsServer

  @endpoint LivebookWeb.Endpoint

  using do
    quote do
      use Livebook.DataCase
      use LivebookWeb.ConnCase

      @moduletag :teams_integration

      alias Livebook.TeamsServer
      alias Livebook.TeamsRPC

      import Livebook.HubHelpers
      import Livebook.TeamsIntegrationCase
    end
  end

  setup_all do
    case TeamsServer.start() do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end

    token = TeamsServer.token()
    url = TeamsServer.url()
    user = TeamsServer.user()
    node = TeamsServer.get_node()

    Application.put_env(:livebook, :teams_url, url, persistent: true)

    {:ok, node: node, token: token, user: user}
  end

  setup context do
    if topics = context[:subscribe_to_hubs_topics] do
      Livebook.Hubs.Broadcasts.subscribe(topics)
    end

    if topics = context[:subscribe_to_teams_topics] do
      Livebook.Teams.Broadcasts.subscribe(topics)
    end

    :ok
  end

  def livebook_teams_auth(%{conn: conn, node: node}) do
    {:ok, deployment_group: deployment_group, org: org, team: team} =
      livebook_teams_zta(%{node: node})

    {conn, code} = authenticate_user_on_teams(conn, node, team)

    {:ok, conn: conn, code: code, deployment_group: deployment_group, org: org, team: team}
  end

  def livebook_teams_zta(%{node: node}) do
    Livebook.Teams.Broadcasts.subscribe([:agents, :clients, :app_deployments, :app_server])
    Livebook.Apps.subscribe()

    {_agent_key, org, deployment_group, team} = Livebook.HubHelpers.create_agent_team_hub(node)

    # we wait until the agent_connected is received by livebook
    hub_id = team.id
    deployment_group_id = to_string(deployment_group.id)
    org_id = to_string(org.id)

    assert_receive {:client_connected, ^hub_id}

    # we wait until the agent_joined is received by livebook
    assert_receive {:agent_joined,
                    %{
                      hub_id: ^hub_id,
                      org_id: ^org_id,
                      deployment_group_id: ^deployment_group_id
                    }}

    start_supervised!({Livebook.ZTA.LivebookTeams, name: LivebookWeb.ZTA, identity_key: team.id})

    {:ok, deployment_group: deployment_group, org: org, team: team}
  end

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

    {Plug.Test.init_test_session(conn, session), code}
  end
end
