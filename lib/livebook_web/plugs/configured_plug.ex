defmodule LivebookWeb.ConfiguredPlug do
  @moduledoc false

  # Runs plugs configured for the :livebook application

  @behaviour Plug

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    case Application.fetch_env!(:livebook, :plugs) do
      [] -> conn
      plugs -> Plug.run(conn, plugs)
    end
  end
end
