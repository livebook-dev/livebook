defmodule Livebook.Text.DeltaTest do
  use ExUnit.Case, async: true

  alias Livebook.Text.Delta
  alias Livebook.Text.Delta.Operation

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

  describe "apply/2" do
    test "prepend" do
      string = "cats"
      delta = Delta.new() |> Delta.insert("fat ")
      assert Delta.apply(delta, string) == "fat cats"
    end

    test "insert in the middle" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(3) |> Delta.insert("'")
      assert Delta.apply(delta, string) == "cat's"
    end

    test "delete" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(1) |> Delta.delete(2)
      assert Delta.apply(delta, string) == "cs"
    end

    test "replace" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(1) |> Delta.delete(2) |> Delta.insert("ar")
      assert Delta.apply(delta, string) == "cars"
    end

    test "retain skips the given number UTF-16 code units" do
      # 🚀 consists of 2 UTF-16 code units, so JavaScript assumes "🚀".length is 2
      string = "🚀 cats"
      # Skip the emoji (2 code unit) and the space (1 code unit)
      delta = Delta.new() |> Delta.retain(3) |> Delta.insert("my ")
      assert Delta.apply(delta, string) == "🚀 my cats"
    end

    test "delete removes the given number UTF-16 code units" do
      # 🚀 consists of 2 UTF-16 code units, so JavaScript assumes "🚀".length is 2
      string = "🚀 cats"
      delta = Delta.new() |> Delta.delete(2)
      assert Delta.apply(delta, string) == " cats"
    end
  end

  describe "diff/2" do
    test "insert" do
      assert Delta.diff("cats", "cat's") ==
               Delta.new() |> Delta.retain(3) |> Delta.insert("'")
    end

    test "delete" do
      assert Delta.diff("cats", "cs") ==
               Delta.new() |> Delta.retain(1) |> Delta.delete(2)
    end

    test "replace" do
      assert Delta.diff("cats", "cars") ==
               Delta.new() |> Delta.retain(2) |> Delta.delete(1) |> Delta.insert("r")
    end

    test "retain skips the given number UTF-16 code units" do
      assert Delta.diff("🚀 cats", "🚀 my cats") ==
               Delta.new() |> Delta.retain(3) |> Delta.insert("my ")
    end

    test "delete removes the given number UTF-16 code units" do
      assert Delta.diff("🚀 cats", " cats") ==
               Delta.new() |> Delta.delete(2)
    end

    test "diffs within changed lines" do
      assert Delta.diff("cats\ndogs\nbirds", "cats\ndogs\nbirds") == Delta.new()

      assert Delta.diff("cats\ndogs\nbirds", "cats\ndog\nbirds") ==
               Delta.new() |> Delta.retain(8) |> Delta.delete(1)

      assert Delta.diff("cats\ndogs", "cats\ndogs\n") ==
               Delta.new() |> Delta.retain(9) |> Delta.insert("\n")

      assert Delta.diff("cats\ndogs\nbirds", "cats\nbirds") ==
               Delta.new() |> Delta.retain(5) |> Delta.delete(5)

      assert Delta.diff("cats\nbirds", "cats\ndogs\nbirds") ==
               Delta.new() |> Delta.retain(5) |> Delta.insert("dogs\n")
    end

    test "keeps common characters when lines are split" do
      assert Delta.diff("foo(a, b)\nx", "foo(\n  a,\n  b\n)\nx") ==
               Delta.new()
               |> Delta.retain(4)
               |> Delta.insert("\n  ")
               |> Delta.retain(2)
               |> Delta.insert("\n ")
               |> Delta.retain(2)
               |> Delta.insert("\n")
    end

    test "inserts or deletes everything for an empty string" do
      assert Delta.diff("", "cats\ndogs") == Delta.new() |> Delta.insert("cats\ndogs")
      assert Delta.diff("cats\ndogs", "") == Delta.new() |> Delta.delete(9)
    end

    test "replaces long lines as a whole" do
      long_line = ~s(data = "#{String.duplicate("a", 2000)}"\n)
      source = "x = 1\n" <> long_line <> "y = 2"

      # Other lines are still diffed precisely.
      assert Delta.diff(source, "x = 1\n" <> long_line <> "y = 3") ==
               Delta.new() |> Delta.retain(2020) |> Delta.insert("3") |> Delta.delete(1)

      new_long_line = String.replace(long_line, "aaaa", "aaba", global: false)

      assert Delta.diff(source, "x = 1\n" <> new_long_line <> "y = 2") ==
               Delta.new() |> Delta.retain(6) |> Delta.insert(new_long_line) |> Delta.delete(2010)
    end

    test "replaces the whole string when there are many lines" do
      source1 = Enum.map_join(1..1500, "\n", &"x#{&1}")
      source2 = Enum.map_join(1..1500, "\n", &"y#{&1}")

      assert Delta.diff(source1, source2) ==
               Delta.new() |> Delta.insert(source2) |> Delta.delete(String.length(source1))
    end
  end
end
