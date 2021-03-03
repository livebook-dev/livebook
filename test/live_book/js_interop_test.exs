defmodule Livebook.JSInteropTest do
  use ExUnit.Case, async: true

  alias Livebook.{JSInterop, Delta}

  describe "apply_delta_to_string/2" do
    test "prepend" do
      string = "cats"
      delta = Delta.new() |> Delta.insert("fat ")
      assert JSInterop.apply_delta_to_string(delta, string) == "fat cats"
    end

    test "insert in the middle" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(3) |> Delta.insert("'")
      assert JSInterop.apply_delta_to_string(delta, string) == "cat's"
    end

    test "delete" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(1) |> Delta.delete(2)
      assert JSInterop.apply_delta_to_string(delta, string) == "cs"
    end

    test "replace" do
      string = "cats"
      delta = Delta.new() |> Delta.retain(1) |> Delta.delete(2) |> Delta.insert("ar")
      assert JSInterop.apply_delta_to_string(delta, string) == "cars"
    end

    test "retain skips the given number UTF-16 code units" do
      # 🚀 consists of 2 UTF-16 code units, so JavaScript assumes "🚀".length is 2
      string = "🚀 cats"
      # Skip the emoji (2 code unit) and the space (1 code unit)
      delta = Delta.new() |> Delta.retain(3) |> Delta.insert("my ")
      assert JSInterop.apply_delta_to_string(delta, string) == "🚀 my cats"
    end

    test "delete removes the given number UTF-16 code units" do
      # 🚀 consists of 2 UTF-16 code units, so JavaScript assumes "🚀".length is 2
      string = "🚀 cats"
      delta = Delta.new() |> Delta.delete(2)
      assert JSInterop.apply_delta_to_string(delta, string) == " cats"
    end
  end
end
