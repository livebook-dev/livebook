defmodule Livebook.Runtime.AttachedTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime

  describe "Runtime.connect/1" do
    test "given an invalid node returns an error" do
      runtime = Runtime.Attached.new(:nonexistent@node)
      assert {:error, "node :nonexistent@node is unreachable"} = Runtime.connect(runtime)
    end
  end
end
