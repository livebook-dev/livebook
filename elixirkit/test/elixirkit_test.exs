defmodule ElixirkitTest do
  use ExUnit.Case
  doctest Elixirkit

  test "greets the world" do
    assert Elixirkit.hello() == :world
  end
end
