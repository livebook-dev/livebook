defmodule Example.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      ExampleWeb.Telemetry,
      {Phoenix.PubSub, name: Example.PubSub},
      {ElixirKit.PubSub,
       connect: System.get_env("ELIXIRKIT_PUBSUB") || :ignore,
       on_exit: fn -> System.stop() end},
      ExampleWeb.Endpoint,
      {Task,
       fn ->
         ElixirKit.PubSub.broadcast("messages", "ready")
       end}
    ]

    opts = [strategy: :one_for_one, name: Example.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    ExampleWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
