defmodule Livebook.DeltaTest do
  use ExUnit.Case, async: true

  alias Livebook.Delta
  alias Livebook.Delta.Operation

  doctest Delta

  describe "append/2" do
    test "ignores empty operations" do
      assert Delta.new() |> Delta.append({:insert, ""}) |> Delta.operations() == []
      assert Delta.new() |> Delta.append({:retain, 0}) |> Delta.operations() == []
      assert Delta.new() |> Delta.append({:delete, 0}) |> Delta.operations() == []
    end

    test "given empty delta just appends the operation" do
      delta = Delta.new()
      op = Operation.insert("cats")
      assert delta |> Delta.append(op) |> Delta.operations() == [insert: "cats"]
    end

    test "merges consecutive inserts" do
      delta = Delta.new() |> Delta.insert("cats")
      op = Operation.insert(" rule")
      assert delta |> Delta.append(op) |> Delta.operations() == [insert: "cats rule"]
    end

    test "merges consecutive retains" do
      delta = Delta.new() |> Delta.retain(2)
      op = Operation.retain(2)
      assert delta |> Delta.append(op) |> Delta.operations() == [retain: 4]
    end

    test "merges consecutive delete" do
      delta = Delta.new() |> Delta.delete(2)
      op = Operation.delete(2)
      assert delta |> Delta.append(op) |> Delta.operations() == [delete: 4]
    end

    test "given insert appended after delete, swaps the operations" do
      delta = Delta.new() |> Delta.delete(2)
      op = Operation.insert("cats")
      assert delta |> Delta.append(op) |> Delta.operations() == [insert: "cats", delete: 2]
    end
  end
end
