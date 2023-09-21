defmodule Livebook.TestModules.Hidden do
  def visible, do: :ok

  @doc false
  def hidden, do: :ok
end
