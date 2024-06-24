defmodule LivebookWeb.ConnCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      import Plug.Conn
      import Phoenix.ConnTest
      import Livebook.Factory
      import LivebookWeb.ConnCase

      use LivebookWeb, :verified_routes

      # The default endpoint for testing
      @endpoint LivebookWeb.Endpoint
    end
  end

  setup tags do
    conn = Phoenix.ConnTest.build_conn()

    conn =
      if authentication = tags[:authentication] do
        with_authentication(conn, authentication)
      else
        conn
      end

    [conn: conn]
  end

  def with_authentication(conn, authentication) do
    Plug.Test.init_test_session(conn, authentication_test_override: authentication)
  end
end
