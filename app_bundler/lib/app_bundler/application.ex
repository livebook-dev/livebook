defmodule AppBundler.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {Registry, keys: :duplicate, name: AppBundler.Registry}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: AppBundler.Supervisor)
  end
end
