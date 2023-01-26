defmodule Livebook.EnterpriseIntegrationCase do
  use ExUnit.CaseTemplate

  alias Livebook.EnterpriseServer

  using do
    quote do
      use LivebookWeb.ConnCase

      @moduletag :enterprise_integration

      import Livebook.EnterpriseIntegrationCase,
        only: [start_new_instance: 1, stop_new_instance: 1]

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

  def start_new_instance(name) do
    suffix = Ecto.UUID.generate() |> :erlang.phash2() |> to_string()
    app_port = Enum.random(1000..9000) |> to_string()

    {:ok, _} =
      EnterpriseServer.start(name,
        env: %{
          "DATABASE_URL" =>
            "postgres://postgres:postgres@localhost:5432/enterprise_integration_#{suffix}"
        },
        app_port: app_port
      )
  end

  def stop_new_instance(name) do
    EnterpriseServer.disconnect(name)
    EnterpriseServer.drop_database(name)
  end
end
