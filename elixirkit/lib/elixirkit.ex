defmodule ElixirKit do
  def start do
    Supervisor.start_child(ElixirKit.Supervisor, {ElixirKit.Server, self()})
  end
end
