defmodule Livebook.Evaluator.ObjecTrackerTest do
  use ExUnit.Case, async: true

  alias Livebook.Evaluator.ObjectTracker

  setup do
    {:ok, object_tracker} = start_supervised(ObjectTracker)
    %{object_tracker: object_tracker}
  end

  test "executes hooks when all object pointers are released", %{object_tracker: object_tracker} do
    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref1})
    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref2})

    parent = self()

    ObjectTracker.add_release_hook(object_tracker, :object1, fn ->
      send(parent, :object1_released)
    end)

    ObjectTracker.remove_pointer(object_tracker, {self(), :ref1})
    ObjectTracker.remove_pointer(object_tracker, {self(), :ref2})

    assert_receive :object1_released
  end

  test "does not execute hooks when other pointers still point to the object",
       %{object_tracker: object_tracker} do
    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref1})
    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref2})

    parent = self()

    ObjectTracker.add_release_hook(object_tracker, :object1, fn ->
      send(parent, :object1_released)
    end)

    ObjectTracker.remove_pointer(object_tracker, {self(), :ref1})

    refute_receive :object1_released
  end

  test "removes a pointer if its process terminates", %{object_tracker: object_tracker} do
    pointer_pid =
      spawn(fn ->
        receive do
          :stop -> :ok
        end
      end)

    ObjectTracker.add_pointer(object_tracker, :object1, {pointer_pid, :ref1})

    parent = self()

    ObjectTracker.add_release_hook(object_tracker, :object1, fn ->
      send(parent, :object1_released)
    end)

    send(pointer_pid, :stop)
    assert_receive :object1_released
  end
end
