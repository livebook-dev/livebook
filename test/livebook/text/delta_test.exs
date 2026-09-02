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

    test "short-circuits large payloads with a single replace" do
      # Regression test for livebook#2916. When either input exceeds the
      # configured `large_payload_threshold/0`, diff/2 must bypass Myers
      # entirely and emit a single replace. We verify three properties:
      #   1. The returned delta still applies to the original and produces
      #      the new string (semantic correctness).
      #   2. The diff completes quickly (orders of magnitude faster than
      #      the baseline Myers path used to take on similar inputs).
      #   3. The delta is a single replace (no retains), confirming the
      #      short-circuit is wired in.
      threshold = Delta.large_payload_threshold()
      base = String.duplicate("a", threshold + 1)
      modified = String.replace_prefix(base, "a", "A")

      {time_us, delta} = :timer.tc(fn -> Delta.diff(base, modified) end)

      assert time_us < 1_000_000,
             "expected diff/2 to finish quickly for a large payload, got #{div(time_us, 1000)}ms"

      assert Delta.apply(delta, base) == modified

      assert Delta.operations(delta) == [
               {:insert, "A" <> String.duplicate("a", threshold)},
               {:delete, threshold + 1}
             ]
    end

    test "large payload edges keep the canonical ops shape" do
      threshold = Delta.large_payload_threshold()
      base = String.duplicate("a", threshold + 1)

      # Identical large payloads produce an empty delta, like the Myers path.
      assert Delta.diff(base, base) == Delta.new()

      # A large deletion emits only the delete op, like the Myers path.
      delta = Delta.diff(base, "")
      assert Delta.apply(delta, base) == ""
      assert Delta.operations(delta) == [{:delete, threshold + 1}]

      # A large insertion into an empty string emits only the insert op,
      # without a zero-length delete, like the Myers path.
      delta = Delta.diff("", base)
      assert Delta.apply(delta, "") == base
      assert Delta.operations(delta) == [{:insert, base}]
    end
  end
end
