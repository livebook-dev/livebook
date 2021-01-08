defmodule LiveBook.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      LiveBookWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: LiveBook.PubSub},
      # Start the supervisor dynamically managing sessions
      LiveBook.SessionSupervisor,
      # Start the Endpoint (http/https)
      LiveBookWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: LiveBook.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    LiveBookWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
