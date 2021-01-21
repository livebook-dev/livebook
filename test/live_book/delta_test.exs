defmodule LiveBook.DeltaTest do
  use ExUnit.Case, async: true

  alias LiveBook.Delta
  alias LiveBook.Delta.Operation

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

  describe "apply_to_string/2" do
    test "prepend" do
      string = "cats"
      delta = Delta.new() |> Delta.insert("fat ")
      assert Delta.apply_to_string(delta, string) == "fat cats"
    end

    test "insert in the middle" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(3) |> Delta.insert("'")
      assert Delta.apply_to_string(delta, string) == "cat's"
    end

    test "delete" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(1) |> Delta.delete(2)
      assert Delta.apply_to_string(delta, string) == "cs"
    end

    test "replace" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(1) |> Delta.delete(2) |> Delta.insert("ar")
      assert Delta.apply_to_string(delta, string) == "cars"
    end
  end
end
