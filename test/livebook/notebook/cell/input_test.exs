defmodule Livebook.Notebook.Cell.InputText do
  use ExUnit.Case, async: true

  alias Livebook.Notebook.Cell.Input

  describe "validate/1" do
    test "given text input allows any value" do
      input = %{Input.new() | type: :text, value: "some 🐈 text"}
      assert Input.validate(input) == :ok
    end

    test "given url input allows full urls" do
      input = %{Input.new() | type: :url, value: "https://example.com"}
      assert Input.validate(input) == :ok

      input = %{Input.new() | type: :url, value: "https://example.com/some/path"}
      assert Input.validate(input) == :ok

      input = %{Input.new() | type: :url, value: ""}
      assert Input.validate(input) == {:error, "not a valid URL"}

      input = %{Input.new() | type: :url, value: "example.com"}
      assert Input.validate(input) == {:error, "not a valid URL"}

      input = %{Input.new() | type: :url, value: "https://"}
      assert Input.validate(input) == {:error, "not a valid URL"}
    end

    test "given number input allows integers and floats" do
      input = %{Input.new() | type: :number, value: "-12"}
      assert Input.validate(input) == :ok

      input = %{Input.new() | type: :number, value: "3.14"}
      assert Input.validate(input) == :ok

      input = %{Input.new() | type: :number, value: ""}
      assert Input.validate(input) == {:error, "not a valid number"}

      input = %{Input.new() | type: :number, value: "1."}
      assert Input.validate(input) == {:error, "not a valid number"}

      input = %{Input.new() | type: :number, value: ".0"}
      assert Input.validate(input) == {:error, "not a valid number"}

      input = %{Input.new() | type: :number, value: "-"}
      assert Input.validate(input) == {:error, "not a valid number"}
    end

    test "given color input allows valid hex colors" do
      input = %{Input.new() | type: :color, value: "#111111"}
      assert Input.validate(input) == :ok

      input = %{Input.new() | type: :color, value: "ABCDEF"}
      assert Input.validate(input) == {:error, "not a valid hex color"}
    end

    test "given range input allows numbers in the configured range" do
      input = %{Input.new() | type: :range, value: "0", props: %{min: -5, max: 5, step: 1}}
      assert Input.validate(input) == :ok

      input = %{Input.new() | type: :range, value: "", props: %{min: -5, max: 5, step: 1}}
      assert Input.validate(input) == {:error, "not a valid number"}

      input = %{Input.new() | type: :range, value: "-10", props: %{min: -5, max: 5, step: 1}}
      assert Input.validate(input) == {:error, "number too small"}

      input = %{Input.new() | type: :range, value: "10", props: %{min: -5, max: 5, step: 1}}
      assert Input.validate(input) == {:error, "number too big"}
    end
  end

  describe "invalidated?/2" do
    test "returns false if only the type changes" do
      input = %{Input.new() | type: :text}
      updated_input = %{input | type: :url}

      refute Input.invalidated?(updated_input, input)
    end

    test "returns true if the name changes" do
      input = %{Input.new() | name: "Name"}
      updated_input = %{input | name: "Full name"}

      assert Input.invalidated?(updated_input, input)
    end

    test "returns true if the value changes" do
      input = %{Input.new() | value: "Jake Peralta"}
      updated_input = %{input | value: "Amy Santiago"}

      assert Input.invalidated?(updated_input, input)
    end
  end

  describe "reactive_change?/2" do
    test "returns false if the input is not reactive" do
      input = %{Input.new() | reactive: false, value: "Jake Peralta"}
      updated_input = %{input | value: "Amy Santiago"}

      refute Input.reactive_update?(updated_input, input)
    end

    test "returns true if the input is reactive and value changes" do
      input = %{Input.new() | reactive: true, value: "Jake Peralta"}
      updated_input = %{input | value: "Amy Santiago"}

      assert Input.reactive_update?(updated_input, input)
    end

    test "returns false if the new value is invalid" do
      input = %{Input.new() | reactive: true, type: :number, value: "10"}
      updated_input = %{input | value: "invalid"}

      refute Input.reactive_update?(updated_input, input)
    end
  end
end
