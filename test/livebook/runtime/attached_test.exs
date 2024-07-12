defmodule Livebook.Runtime.AttachedTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime

  describe "Runtime.connect/1" do
    test "given an invalid node returns an error" do
      runtime = Runtime.Attached.new(:nonexistent@node)
      pid = Runtime.connect(runtime)

      assert_receive {:runtime_connect_done, ^pid,
                      {:error, "node :nonexistent@node is unreachable"}}
    end
  end
end
