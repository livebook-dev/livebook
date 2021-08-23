defmodule Livebook.TestModules.Hidden do
  @moduledoc false

  def visible, do: :ok

  @doc false
  def hidden, do: :ok
end
