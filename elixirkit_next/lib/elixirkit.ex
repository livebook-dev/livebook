defmodule ElixirKit do
  def start do
    Supervisor.start_child(ElixirKit.Supervisor, {ElixirKit.Server, self()})
  end

  def publish(name, data) do
    GenServer.call(ElixirKit.Server, {:send_event, name, data})
  end
end
