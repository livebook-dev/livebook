defmodule Livebook.EnterpriseIntegrationCase do
  use ExUnit.CaseTemplate

  alias LivebookTest.EnterpriseServer

  using do
    quote do
      use LivebookWeb.ConnCase

      @moduletag :enterprise_integration

      alias LivebookTest.EnterpriseServer
    end
  end

  setup_all do
    EnterpriseServer.start()

    {:ok, url: EnterpriseServer.url(), token: EnterpriseServer.token()}
  end
end
