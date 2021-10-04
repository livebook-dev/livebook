defmodule LivebookWeb.ConfiguredPlug do
  @moduledoc false

  # Runs plugs configured for the :livebook application

  @behaviour Plug

  @impl true
  def init(_opts) do
    plugs = Application.get_env(:livebook, :plugs, [])
    %{plugs: plugs}
  end

  @impl true
  def call(conn, %{plugs: []}), do: conn
  def call(conn, %{plugs: plugs}), do: Plug.run(conn, plugs)
end
