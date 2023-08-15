defmodule Livebook.TeamsIntegrationCase do
  use ExUnit.CaseTemplate

  alias Livebook.TeamsServer

  using do
    quote do
      use Livebook.DataCase
      use LivebookWeb.ConnCase

      @moduletag :teams_integration

      alias Livebook.TeamsServer

      import Livebook.HubHelpers
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
end
