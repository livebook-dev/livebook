defmodule Livebook.DeltaTest do
  use ExUnit.Case, async: true

  alias Livebook.Delta
  alias Livebook.Delta.Operation

  doctest Delta

  describe "append/2" do
    test "ignores empty operations" do
      assert Delta.append(Delta.new(), {:insert, ""}) == %Delta{ops: []}
      assert Delta.append(Delta.new(), {:retain, 0}) == %Delta{ops: []}
      assert Delta.append(Delta.new(), {:delete, 0}) == %Delta{ops: []}
    end

    test "given empty delta just appends the operation" do
      delta = Delta.new()
      op = Operation.insert("cats")
      assert Delta.append(delta, op) == %Delta{ops: [insert: "cats"]}
    end

    test "merges consecutive inserts" do
      delta = Delta.new() |> Delta.insert("cats")
      op = Operation.insert(" rule")
      assert Delta.append(delta, op) == %Delta{ops: [insert: "cats rule"]}
    end

    test "merges consecutive retains" do
      delta = Delta.new() |> Delta.retain(2)
      op = Operation.retain(2)
      assert Delta.append(delta, op) == %Delta{ops: [retain: 4]}
    end

    test "merges consecutive delete" do
      delta = Delta.new() |> Delta.delete(2)
      op = Operation.delete(2)
      assert Delta.append(delta, op) == %Delta{ops: [delete: 4]}
    end

    test "given insert appended after delete, swaps the operations" do
      delta = Delta.new() |> Delta.delete(2)
      op = Operation.insert("cats")
      assert Delta.append(delta, op) == %Delta{ops: [insert: "cats", delete: 2]}
    end
  end
end
