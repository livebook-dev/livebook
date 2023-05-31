defmodule Livebook.Runtime.ErlDist.NodeManagerTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime
  alias Livebook.Runtime.ErlDist.{NodeManager, RuntimeServer}

  test "terminates when the last runtime server terminates" do
    # We use a standalone runtime, so that we have an isolated node
    # with its own node manager
    assert {:ok, %{node: node, server_pid: server1} = runtime} =
             Runtime.ElixirStandalone.new() |> Runtime.connect()

    Runtime.take_ownership(runtime)

    manager_pid = :erpc.call(node, Process, :whereis, [Livebook.Runtime.ErlDist.NodeManager])
    ref = Process.monitor(manager_pid)

    server2 = NodeManager.start_runtime_server(node)

    RuntimeServer.stop(server1)
    RuntimeServer.stop(server2)

    assert_receive {:DOWN, ^ref, :process, _, _}
  end
end
