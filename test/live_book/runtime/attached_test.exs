defmodule LiveBook.Runtime.AttachedTest do
  use ExUnit.Case, async: true

  alias LiveBook.Runtime

  describe "init/1" do
    test "given a running node returns a new runtime struct" do
      self_node = node()
      assert {:ok, %{node: ^self_node}} = Runtime.Attached.init(self_node)
    end

    test "given an invalid node returns an error" do
      assert {:error, :unreachable} = Runtime.Attached.init(:nonexistent@node)
    end
  end
end
