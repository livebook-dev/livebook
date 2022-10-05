defmodule Livebook.EnterpriseIntegrationCase do
  use ExUnit.CaseTemplate

  alias LivebookTest.Integration.EnterpriseServer

  using do
    quote do
      use LivebookWeb.ConnCase

      @moduletag :enterprise_integration

      alias LivebookTest.Integration.EnterpriseServer
    end
  end

  setup_all do
    EnterpriseServer.start()

    :ok
  end
end
