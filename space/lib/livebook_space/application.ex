defmodule LivebookSpace.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {Plug.Cowboy, scheme: :http, plug: LivebookSpaceWeb.Plug, options: [port: 4000]}
    ]

    opts = [strategy: :one_for_one, name: LivebookSpace.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
