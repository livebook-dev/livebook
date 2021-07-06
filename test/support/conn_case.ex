defmodule LivebookWeb.ConnCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      import Plug.Conn
      import Phoenix.ConnTest
      import LivebookWeb.ConnCase

      alias unquote(Livebook.Config.router_helpers_module), as: Routes

      # The default endpoint for testing
      @endpoint LivebookWeb.Endpoint
    end
  end

  setup _tags do
    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
