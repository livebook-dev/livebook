defmodule LiveBookWeb.ConnCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      import Plug.Conn
      import Phoenix.ConnTest
      import LiveBookWeb.ConnCase

      alias LiveBookWeb.Router.Helpers, as: Routes

      # The default endpoint for testing
      @endpoint LiveBookWeb.Endpoint
    end
  end

  setup _tags do
    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
