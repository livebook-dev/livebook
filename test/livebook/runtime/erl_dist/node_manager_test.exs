defmodule Livebook.Runtime.ErlDist.NodeManagerTest do
  use ExUnit.Case, async: false

  alias Livebook.Runtime.ErlDist.{NodeManager, RuntimeServer}

  test "terminates when the last runtime server terminates" do
    {:ok, manager_pid} =
      start_supervised({NodeManager, [unload_modules_on_termination: false, anonymous: true]})

    server1 = NodeManager.start_runtime_server(manager_pid)
    server2 = NodeManager.start_runtime_server(manager_pid)

    ref = Process.monitor(manager_pid)

    RuntimeServer.stop(server1)
    assert Process.alive?(manager_pid)

    RuntimeServer.stop(server2)
    assert_receive {:DOWN, ^ref, :process, _, _}
  end
end
