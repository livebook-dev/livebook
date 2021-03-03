defmodule Livebook.Runtime.AttachedTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime

  describe "init/1" do
    test "given an invalid node returns an error" do
      assert {:error, :unreachable} = Runtime.Attached.init(:nonexistent@node)
    end
  end
end
