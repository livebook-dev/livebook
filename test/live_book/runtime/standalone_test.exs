defmodule LiveBook.Runtime.StandaloneTest do
  use ExUnit.Case, async: true

  alias LiveBook.Runtime

  describe "init/1" do
    test "starts a new Elixir runtime in distribution mode and ties its lifetime to the given owner process" do
      owner =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      assert {:ok, %{node: node}} = Runtime.Standalone.init(owner)

      # Make sure the node is running.
      Node.monitor(node, true)
      assert :pong = Node.ping(node)

      # Tell the owner process to stop.
      send(owner, :stop)

      # Once the owner process terminates, the node should terminate as well.
      assert_receive {:nodedown, ^node}
    end

    test "loads necessary modules and starts manager process" do
      assert {:ok, %{node: node}} = Runtime.Standalone.init(self())

      assert evaluator_module_loaded?(node)
      assert manager_started?(node)
    end
  end

  test "Runtime.disconnect/1 makes the node terminate" do
    assert {:ok, %{node: node} = runtime} = Runtime.Standalone.init(self())

    # Make sure the node is running.
    Node.monitor(node, true)
    assert :pong = Node.ping(node)

    Runtime.disconnect(runtime)

    assert_receive {:nodedown, ^node}
  end

  defp evaluator_module_loaded?(node) do
    :rpc.call(node, :code, :is_loaded, [LiveBook.Evaluator]) != false
  end

  defp manager_started?(node) do
    :rpc.call(node, Process, :whereis, [LiveBook.Runtime.ErlDist.Manager]) != nil
  end
end
