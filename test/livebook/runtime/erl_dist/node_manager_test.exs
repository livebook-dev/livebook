defmodule Livebook.Runtime.ErlDist.NodeManagerTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime
  alias Livebook.Runtime.ErlDist.NodeManager
  alias Livebook.Runtime.ErlDist.RuntimeServer

  test "terminates when the last runtime server terminates" do
    # We use a standalone runtime, so that we have an isolated node
    # with its own node manager
    pid = Runtime.Standalone.new() |> Runtime.connect()
    assert_receive {:runtime_connect_done, ^pid, {:ok, runtime}}
    %{node: node, server_pid: server1} = runtime

    Runtime.take_ownership(runtime)

    manager_pid = :erpc.call(node, Process, :whereis, [Livebook.Runtime.ErlDist.NodeManager])
    ref = Process.monitor(manager_pid)

    {:ok, server2} = NodeManager.start_runtime_server(node)

    RuntimeServer.stop(server1)
    RuntimeServer.stop(server2)

    assert_receive {:DOWN, ^ref, :process, _, _}
  end
end
