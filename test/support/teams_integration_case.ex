defmodule Livebook.TeamsIntegrationCase do
  use ExUnit.CaseTemplate

  alias Livebook.TeamsServer

  using do
    quote do
      use Livebook.DataCase
      use LivebookWeb.ConnCase

      @moduletag :teams_integration

      alias Livebook.TeamsServer
      alias Livebook.TeamsRPC

      import Livebook.HubHelpers
      import Livebook.TeamsIntegrationHelper
    end
  end

  setup_all do
    case TeamsServer.start() do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end

    url = TeamsServer.url()
    node = TeamsServer.get_node()

    Application.put_env(:livebook, :teams_url, url, persistent: true)

    {:ok, node: node}
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
end
