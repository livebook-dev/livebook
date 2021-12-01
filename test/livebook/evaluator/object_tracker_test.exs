defmodule Livebook.Evaluator.ObjecTrackerTest do
  use ExUnit.Case, async: true

  alias Livebook.Evaluator.ObjectTracker

  setup do
    {:ok, object_tracker} = start_supervised(ObjectTracker)
    %{object_tracker: object_tracker}
  end

  test "executes :send hooks when all object pointers are released",
       %{object_tracker: object_tracker} do
    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref1})
    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref2})

    ObjectTracker.add_release_hook(object_tracker, :object1, {:send, self(), :object1_released})

    ObjectTracker.remove_pointer(object_tracker, {self(), :ref1})
    ObjectTracker.remove_pointer(object_tracker, {self(), :ref2})

    assert_receive :object1_released
  end

  test "does not execute hooks when other pointers still point to an object",
       %{object_tracker: object_tracker} do
    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref1})
    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref2})

    ObjectTracker.add_release_hook(object_tracker, :object1, {:send, self(), :object1_released})

    ObjectTracker.remove_pointer(object_tracker, {self(), :ref1})

    refute_receive :object1_released
  end

  test "executes :kill hooks all object pointers are released", %{object_tracker: object_tracker} do
    pid = spawn(fn -> Process.sleep(:infinity) end)
    monitor_ref = Process.monitor(pid)

    ObjectTracker.add_pointer(object_tracker, :object1, {self(), :ref1})
    ObjectTracker.add_release_hook(object_tracker, :object1, {:kill, pid})
    ObjectTracker.remove_pointer(object_tracker, {self(), :ref1})

    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :shutdown}
  end

  test "removes a pointer if its process terminates", %{object_tracker: object_tracker} do
    parent =
      spawn(fn ->
        receive do
          :stop -> :ok
        end
      end)

    ObjectTracker.add_pointer(object_tracker, :object1, {parent, :ref1})
    ObjectTracker.add_release_hook(object_tracker, :object1, {:send, self(), :object1_released})

    send(parent, :stop)
    assert_receive :object1_released
  end
end
