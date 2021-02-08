defmodule LiveBook.Remote.InitializationCounter do
  @moduledoc false

  # A simple counter process responsible for keeping track
  # of remote node initializations.

  use Agent

  @name __MODULE__

  def start_link(_opts \\ []) do
    Agent.start_link(fn -> 0 end, name: @name)
  end

  @spec value(node()) :: non_neg_integer()
  def value(node) do
    Agent.get({@name, node}, & &1)
  end

  @spec increment(node()) :: :ok
  def increment(node) do
    Agent.update({@name, node}, &(&1 + 1))
  end

  @spec decrement(node()) :: :ok
  def decrement(node) do
    Agent.update({@name, node}, &(&1 - 1))
  end
end
