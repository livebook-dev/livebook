defmodule Livebook.EPMDTest do
  use ExUnit.Case, async: true

  describe "with epmd" do
    @describetag :with_epmd
    test "has a random dist port" do
      assert Livebook.EPMD.dist_port() == 0
    end
  end

  describe "without epmd" do
    @describetag :without_epmd

    test "has a custom dist port" do
      assert Livebook.EPMD.dist_port() != 0
    end
  end
end
