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

  def authenticate_user_on_teams(conn, node, team) do
    response =
      conn
      |> LivebookWeb.ConnCase.with_authorization(team.id)
      |> get("/")
      |> html_response(200)

    [_, location] = Regex.run(~r/URL\("(.*?)"\)/, response)
    uri = URI.parse(location)
    %{"code" => code} = URI.decode_query(uri.query)

    Livebook.HubHelpers.erpc_call(node, :allow_auth_request, [code])

    session =
      conn
      |> LivebookWeb.ConnCase.with_authorization(team.id)
      |> get("/", %{teams_identity: "", code: code})
      |> Plug.Conn.get_session()

    {Plug.Test.init_test_session(conn, session), code}
  end
end
