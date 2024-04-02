defmodule Livebook.Runtime.Evaluator.ClientTrackerTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.Evaluator.ClientTracker

  setup do
    {:ok, client_tracker} = start_supervised(ClientTracker)
    %{client_tracker: client_tracker}
  end

  test "sends client joins to monitoring processes", %{client_tracker: client_tracker} do
    ClientTracker.monitor_clients(client_tracker, self())

    clients = [{"c1", user_info()}, {"c2", user_info()}]
    ClientTracker.register_clients(client_tracker, clients)

    assert_receive {:client_join, "c1"}
    assert_receive {:client_join, "c2"}
  end

  test "sends client leaves to monitoring processes", %{client_tracker: client_tracker} do
    ClientTracker.monitor_clients(client_tracker, self())

    clients = [{"c1", user_info()}, {"c2", user_info()}]
    ClientTracker.register_clients(client_tracker, clients)

    ClientTracker.unregister_clients(client_tracker, ["c1"])
    assert_receive {:client_leave, "c1"}

    ClientTracker.unregister_clients(client_tracker, ["c2"])
    assert_receive {:client_leave, "c2"}
  end

  test "returns existing client joins when monitoring starts", %{client_tracker: client_tracker} do
    clients = [{"c1", user_info()}, {"c2", user_info()}]
    ClientTracker.register_clients(client_tracker, clients)
    ClientTracker.unregister_clients(client_tracker, ["c1"])

    assert ClientTracker.monitor_clients(client_tracker, self()) == ["c2"]
  end

  defp user_info() do
    %{
      id: "u1",
      name: "Jake Peralta",
      email: nil,
      source: :session
    }
  end
end
