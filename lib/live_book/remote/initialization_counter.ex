defmodule LiveBook.Remote.InitializationCounter do
  use Agent

  @name __MODULE__

  def start_link(_opts \\ []) do
    Agent.start_link(fn -> 0 end, name: @name)
  end

  def value(node) do
    Agent.get({@name, node}, & &1)
  end

  def increment(node) do
    Agent.update({@name, node}, &(&1 + 1))
  end

  def decrement(node) do
    Agent.update({@name, node}, &(&1 - 1))
  end
end
