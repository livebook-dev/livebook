defmodule Livebook.EPMDTest do
  use ExUnit.Case, async: true

  test "has a custom dist port" do
    assert Livebook.EPMD.dist_port() != 0
  end
end
