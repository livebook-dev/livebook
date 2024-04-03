defmodule Livebook.Runtime.Evaluator.ClientTrackerTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.Evaluator.ClientTracker

  setup do
    {:ok, client_tracker} = start_supervised(ClientTracker)
    %{client_tracker: client_tracker}
  end

  test "sends client joins to monitoring processes", %{client_tracker: client_tracker} do
    ClientTracker.monitor_clients(client_tracker, self())

    ClientTracker.register_clients(client_tracker, ["c1", "c2"])

    assert_receive {:client_join, "c1"}
    assert_receive {:client_join, "c2"}
  end

  test "sends client leaves to monitoring processes", %{client_tracker: client_tracker} do
    ClientTracker.monitor_clients(client_tracker, self())

    ClientTracker.register_clients(client_tracker, ["c1", "c2"])

    ClientTracker.unregister_clients(client_tracker, ["c1"])
    assert_receive {:client_leave, "c1"}

    ClientTracker.unregister_clients(client_tracker, ["c2"])
    assert_receive {:client_leave, "c2"}
  end

  test "returns existing client joins when monitoring starts", %{client_tracker: client_tracker} do
    ClientTracker.register_clients(client_tracker, ["c1", "c2"])
    ClientTracker.unregister_clients(client_tracker, ["c1"])

    assert ClientTracker.monitor_clients(client_tracker, self()) == ["c2"]
  end
end
