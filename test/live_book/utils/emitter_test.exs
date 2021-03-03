defmodule Livebook.Utils.EmitterTest do
  use ExUnit.Case, async: true

  alias Livebook.Utils.Emitter

  describe "emit/2" do
    test "sends the item as a message to the specified process" do
      emitter = Emitter.new(self())
      ref = emitter.ref
      Emitter.emit(emitter, :hey)

      assert_receive {:emitter, ^ref, :hey}
    end
  end

  describe "map/2" do
    test "returns a modified emitter that transforms items before they are sent" do
      emitter = Emitter.new(self())
      ref = emitter.ref
      string_emitter = Emitter.mapper(emitter, &to_string/1)
      Emitter.emit(string_emitter, :hey)

      assert_receive {:emitter, ^ref, "hey"}
    end

    test "supports chaining" do
      emitter = Emitter.new(self())
      ref = emitter.ref
      string_emitter = Emitter.mapper(emitter, &to_string/1)
      duplicate_emitter = Emitter.mapper(string_emitter, fn x -> {x, x} end)
      Emitter.emit(duplicate_emitter, :hey)

      assert_receive {:emitter, ^ref, {"hey", "hey"}}
    end
  end

  test "implements Collectable so that it emits every item" do
    emitter = Emitter.new(self())
    ref = emitter.ref
    for x <- ["a", "b"], into: emitter, do: x

    assert_receive {:emitter, ^ref, "a"}
    assert_receive {:emitter, ^ref, "b"}
  end
end
