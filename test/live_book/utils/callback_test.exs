defmodule LiveBook.Utils.CallbackTest do
  use ExUnit.Case, async: true

  alias LiveBook.Utils.Callback

  test "implements Collectable so that it calls the wrapped function for every item" do
    callback = Callback.new(fn x -> send(self(), x) end)
    for x <- ["a", "b"], into: callback, do: x

    assert_received "a"
    assert_received "b"
  end
end
