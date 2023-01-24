defmodule Livebook.EnterpriseIntegrationCase do
  use ExUnit.CaseTemplate

  alias Livebook.EnterpriseServer

  using do
    quote do
      use LivebookWeb.ConnCase

      @moduletag :enterprise_integration

      alias Livebook.EnterpriseServer
    end
  end

  setup_all do
    case EnterpriseServer.start() do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end

    {:ok,
     url: EnterpriseServer.url(), token: EnterpriseServer.token(), user: EnterpriseServer.user()}
  end
end
