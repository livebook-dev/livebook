defmodule LivebookSpace.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Bandit, scheme: :http, plug: LivebookSpaceWeb.Plug, port: 4000}
    ]

    opts = [strategy: :one_for_one, name: LivebookSpace.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
