defmodule Livebook.Runtime.Evaluator.ObjecTrackerTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.Evaluator.ObjectTracker

  setup do
    {:ok, object_tracker} = start_supervised(ObjectTracker)
    %{object_tracker: object_tracker}
  end

  test "monitor/5 returns an error when the given object doesn't exist",
       %{object_tracker: object_tracker} do
    assert {:error, :bad_object} =
             ObjectTracker.monitor(object_tracker, :object1, self(), :object1_released, false)
  end

  test "sends scheduled monitor messages when all object references are released",
       %{object_tracker: object_tracker} do
    ObjectTracker.add_reference(object_tracker, :object1, {self(), :ref1})
    ObjectTracker.add_reference(object_tracker, :object1, {self(), :ref2})

    ObjectTracker.monitor(object_tracker, :object1, self(), :object1_released, false)

    ObjectTracker.remove_reference_sync(object_tracker, {self(), :ref1})
    ObjectTracker.remove_reference_sync(object_tracker, {self(), :ref2})

    assert_receive :object1_released
  end

  test "does not execute hooks when other references still point to the object",
       %{object_tracker: object_tracker} do
    ObjectTracker.add_reference(object_tracker, :object1, {self(), :ref1})
    ObjectTracker.add_reference(object_tracker, :object1, {self(), :ref2})

    ObjectTracker.monitor(object_tracker, :object1, self(), :object1_released, false)

    ObjectTracker.remove_reference_sync(object_tracker, {self(), :ref1})

    refute_receive :object1_released
  end

  test "remove_reference_sync/2 awaits for monitor acknowledgements",
       %{object_tracker: object_tracker} do
    ObjectTracker.add_reference(object_tracker, :object1, {self(), :ref1})

    ObjectTracker.monitor(object_tracker, :object1, self(), :object1_released1, true)
    ObjectTracker.monitor(object_tracker, :object1, self(), :object1_released2, false)

    myself = self()

    spawn_link(fn ->
      ObjectTracker.remove_reference_sync(object_tracker, {myself, :ref1})
      send(myself, :removed)
    end)

    assert_receive {:object1_released1, ^object_tracker, reply_as}
    assert_receive :object1_released2

    refute_receive :removed
    send(object_tracker, reply_as)
    assert_receive :removed
  end

  test "removes a reference if its process terminates", %{object_tracker: object_tracker} do
    reference_pid =
      spawn(fn ->
        receive do
          :stop -> :ok
        end
      end)

    ObjectTracker.add_reference(object_tracker, :object1, {reference_pid, :ref1})

    ObjectTracker.monitor(object_tracker, :object1, self(), :object1_released, false)

    send(reference_pid, :stop)
    assert_receive :object1_released
  end
end
