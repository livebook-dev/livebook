defmodule LiveBook.Delta.TransformationText do
  use ExUnit.Case, async: true

  alias LiveBook.Delta

  describe "transform" do
    test "insert against insert" do
      a =
        Delta.new()
        |> Delta.insert("A")

      b =
        Delta.new()
        |> Delta.insert("B")

      b_prime_assuming_a_first =
        Delta.new()
        |> Delta.retain(1)
        |> Delta.insert("B")

      b_prime_assuming_b_first =
        Delta.new()
        |> Delta.insert("B")

      assert Delta.transform(a, b, :left) == b_prime_assuming_a_first
      assert Delta.transform(a, b, :right) == b_prime_assuming_b_first
    end

    test "retain against insert" do
      a =
        Delta.new()
        |> Delta.insert("A")

      b =
        Delta.new()
        |> Delta.retain(1)
        # Add insert, so that trailing retain is not trimmed (same in other places)
        |> Delta.insert("B")

      b_prime =
        Delta.new()
        |> Delta.retain(2)
        |> Delta.insert("B")

      assert Delta.transform(a, b, :right) == b_prime
    end

    test "delete against insert" do
      a =
        Delta.new()
        |> Delta.insert("A")

      b =
        Delta.new()
        |> Delta.delete(1)

      b_prime =
        Delta.new()
        |> Delta.retain(1)
        |> Delta.delete(1)

      assert Delta.transform(a, b, :right) == b_prime
    end

    test "insert against delete" do
      a =
        Delta.new()
        |> Delta.delete(1)

      b =
        Delta.new()
        |> Delta.insert("B")

      b_prime =
        Delta.new()
        |> Delta.insert("B")

      assert Delta.transform(a, b, :right) == b_prime
    end

    test "retain against delete" do
      a =
        Delta.new()
        |> Delta.delete(1)

      b =
        Delta.new()
        |> Delta.retain(1)
        |> Delta.insert("B")

      b_prime =
        Delta.new()
        |> Delta.insert("B")

      assert Delta.transform(a, b, :right) == b_prime
    end

    test "delete against delete" do
      a =
        Delta.new()
        |> Delta.delete(1)

      b =
        Delta.new()
        |> Delta.delete(1)

      b_prime = Delta.new()

      assert Delta.transform(a, b, :right) == b_prime
    end

    test "insert against retain" do
      a =
        Delta.new()
        |> Delta.retain(1)
        |> Delta.insert("A")

      b =
        Delta.new()
        |> Delta.insert("B")

      b_prime =
        Delta.new()
        |> Delta.insert("B")

      assert Delta.transform(a, b, :right) == b_prime
    end

    test "retain against retain" do
      a =
        Delta.new()
        |> Delta.retain(1)
        |> Delta.insert("A")

      b =
        Delta.new()
        |> Delta.retain(1)
        |> Delta.insert("B")

      b_prime =
        Delta.new()
        |> Delta.retain(1)
        |> Delta.insert("B")

      assert Delta.transform(a, b, :right) == b_prime
    end

    test "delete against retain" do
      a =
        Delta.new()
        |> Delta.retain(1)

      b =
        Delta.new()
        |> Delta.delete(1)

      b_prime =
        Delta.new()
        |> Delta.delete(1)

      assert Delta.transform(a, b, :right) == b_prime
    end

    test "multiple edits" do
      a =
        Delta.new()
        # Move 2 positions
        |> Delta.retain(2)
        # Insert a word
        |> Delta.insert("aa")
        # Delete a word
        |> Delta.delete(5)

      b =
        Delta.new()
        # Move 1 position
        |> Delta.retain(1)
        # Insert a word
        |> Delta.insert("b")
        # Delete a word
        |> Delta.delete(5)
        # Move 1 position
        |> Delta.retain(1)
        # Insert another word
        |> Delta.insert("bb")

      b_prime_assuming_b_first =
        Delta.new()
        |> Delta.retain(1)
        |> Delta.insert("b")
        |> Delta.delete(1)
        |> Delta.retain(2)
        |> Delta.insert("bb")

      a_prime_assuming_b_first =
        Delta.new()
        |> Delta.retain(2)
        |> Delta.insert("aa")
        |> Delta.delete(1)

      assert Delta.transform(a, b, :right) == b_prime_assuming_b_first
      assert Delta.transform(b, a, :left) == a_prime_assuming_b_first
    end

    test "conflicting appends" do
      a =
        Delta.new()
        |> Delta.retain(3)
        |> Delta.insert("aa")

      b =
        Delta.new()
        |> Delta.retain(3)
        |> Delta.insert("bb")

      b_prime_assuming_b_first =
        Delta.new()
        |> Delta.retain(3)
        |> Delta.insert("bb")

      a_prime_assuming_b_first =
        Delta.new()
        |> Delta.retain(5)
        |> Delta.insert("aa")

      assert Delta.transform(a, b, :right) == b_prime_assuming_b_first
      assert Delta.transform(b, a, :left) == a_prime_assuming_b_first
    end

    test "prepend and append" do
      a =
        Delta.new()
        |> Delta.insert("aa")

      b =
        Delta.new()
        |> Delta.retain(3)
        |> Delta.insert("bb")

      b_prime_assuming_b_first =
        Delta.new()
        |> Delta.retain(5)
        |> Delta.insert("bb")

      a_prime_assuming_b_first =
        Delta.new()
        |> Delta.insert("aa")

      assert Delta.transform(a, b, :right) == b_prime_assuming_b_first
      assert Delta.transform(b, a, :left) == a_prime_assuming_b_first
    end

    test "trailing deletes with different lengths" do
      a =
        Delta.new()
        |> Delta.retain(2)
        |> Delta.delete(1)

      b =
        Delta.new()
        |> Delta.delete(3)

      b_prime =
        Delta.new()
        |> Delta.delete(2)

      a_prime = Delta.new()

      assert Delta.transform(a, b, :right) == b_prime
      assert Delta.transform(b, a, :left) == a_prime
    end
  end
end
