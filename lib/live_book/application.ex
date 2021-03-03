defmodule Livebook.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    ensure_distribution()

    children = [
      # Start the Telemetry supervisor
      LivebookWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Livebook.PubSub},
      # Start the supervisor dynamically managing sessions
      Livebook.SessionSupervisor,
      # Start the server responsible for associating files with sessions
      Livebook.Session.FileGuard,
      # Start the Endpoint (http/https)
      LivebookWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Livebook.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    LivebookWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp ensure_distribution() do
    unless Node.alive?() do
      System.cmd("epmd", ["-daemon"])
      {type, name} = Application.fetch_env!(:livebook, :node_name)
      Node.start(name, type)
    end
  end
end
